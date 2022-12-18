# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data obtained from the authors of
# http://doi.org/10.1016/j.cell.2020.05.032, Fig. S4
# (https://www.cell.com/cell/fulltext/S0092-8674(20)30627-9#figs4)
df_data <- read.csv(file.path("euler", "data.csv"))

# Prepare list of two (empty) subplots: : P(roteins) and M(etabolites)
subplots <- list(Proteins = NA, Metabolites = NA)

# Loop over subplots
for (name in names(subplots)) {
  # Prepare list of three dataframes
  data <- list(NA, NA, NA)
  names(data) <- paste(c("non-COVID-19", "non-Severe", "Severe"), "/\nHealthy")

  # Read columns P1, P2, P3, M1, M2, M3
  first_letter <- substr(name, 1, 1)
  for (i in 1:3) {
    data_i <- df_data[, paste0(first_letter, i)]

    # Save dataframe to list, omitting empty entries
    data[[i]] <- data_i[data_i != ""]
  }

  # Fit data using circular Euler plot
  # https://cran.r-project.org/web/packages/eulerr/vignettes/introduction.html
  fit <- eulerr::euler(data)

  # Fall back to ellipses if data does not fit circular Euler plot
  if (any(abs(fit$residuals / fit$original) > 0.1)) {
    fit <- eulerr::euler(data, shape = "ellipse")
    # Stop if that does not work, either
    stopifnot(all(abs(fit$residuals / fit$original) < 0.1))
  }

  # Generate plot object
  # https://cran.r-project.org/web/packages/eulerr/vignettes/introduction.html
  subplots[[name]] <- suppress_warning(
    "NAs introduced by coercion",
    plot(
      fit,
      main = name,
      fill = "white",
      edges = list(col = "grey"),
      labels = list(fontsize = 8, font = "normal"),
      quantities = list(fontsize = 8, col = c(
        "black", "red", "red", "black", "black", "red", "black"
      )),
    )
  )
}

# Arrange subplots side by side
# https://wilkelab.org/cowplot/articles/plot_grid.html
euler_plot <- cowplot::plot_grid(subplots$Prot, subplots$Metab, nrow = 1) +
  cowplot::draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 1), size = 15) +
  NULL

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("euler", euler_plot, width = 7, height = 3.5)
