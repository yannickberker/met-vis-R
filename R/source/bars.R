# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data obtained from the authors of
# https://doi.org/10.1038/s41586-019-1503-x, Fig. 1B
# (https://www.nature.com/articles/s41586-019-1503-x#Fig1)
df_data <- read.csv(file.path("bars", "data.csv"))

# Give each pair of data point a number to connect corresponding data points
# https://dplyr.tidyverse.org/reference/mutate.html
df_data <- dplyr::mutate(df_data, Pair = dplyr::row_number())

# Convert (Val_TN, Val_Cold) value columns into (Temp, Val) columns
# https://tidyr.tidyverse.org/reference/pivot_longer.html
df_data <- tidyr::pivot_longer(
  df_data,
  cols = tidyr::starts_with("Val"),
  names_to = "Temp",
  values_to = "Val",
  names_transform = list(Temp = \(names) sub("Val_", "", names)),
)

# Reverse order of conditions ("TN" before "Cold")
# https://dplyr.tidyverse.org/reference/mutate.html
df_data <- dplyr::mutate(df_data, Temp = forcats::fct_rev(Temp))

# Merge "BAT" and "Temp" columns
df_data$BAT_Temp <- interaction(df_data$BAT, df_data$Temp)
stopifnot(all(
  levels(df_data$BAT_Temp) == c("High.TN", "Low.TN", "High.Cold", "Low.Cold")
))

# Compute Mean and SD
# https://dplyr.tidyverse.org/reference/summarise.html
df_error <- dplyr::summarise(
  dplyr::group_by(df_data, BAT, BAT_Temp),
  Mean = mean(Val),
  SE = sd(Val) / sqrt(length(Val)),
  .groups = "drop",
)

# Significance bracket coordinates and annotations
df_signif <- data.frame(
  BAT = c("High", "Low"),
  xmin = c("High.TN", "Low.TN"),
  xmax = c("High.Cold", "Low.Cold"),
  y_position = c(280, 270),
  annotations = c("P = 4 \\times ~10^{-4}", "P = 0.15")
)

# Labels
levels <- levels(df_data$BAT_Temp)
labels <- sub("High\\..*", "", levels)
labels <- sub("Low\\.", "", labels)

# Colors
fill_high <- "#9a3d67"
fill_low <- "#5b6e9e"

dark_high <- "#781b45"
dark_low <- "#394c7c"

fills <- ifelse(grepl("TN", levels), "white",
  ifelse(grepl("High", levels), fill_high, fill_low)
)
colors <- ifelse(grepl("High", levels), dark_high, dark_low)

# Generate ggplot2 plot object
# https://ggplot2.tidyverse.org/

# Configure dataframe (columns) and mapping (from columns to aesthetics)
bars_plot <- ggplot(
  data = df_data,
  mapping = aes(x = BAT_Temp, y = Val, color = BAT_Temp, fill = BAT_Temp),
) +

  # Bars
  geom_bar(
    stat = "summary", fun = "mean", position = position_dodge(),
  ) +

  # Significance brackets
  suppress_warning_signif(
    ggsignif::geom_signif(
      data = df_signif,
      mapping = aes(
        color = NULL,
        fill = NULL,
        xmin = xmin,
        xmax = xmax,
        y_position = y_position,
        annotations = as.character(make_tex_labels(annotations)),
      ),
      manual = TRUE,
      parse = TRUE,
      show.legend = FALSE,
    )
  ) +

  # Lines connecting data points
  geom_line(
    mapping = aes(group = Pair),
    show.legend = FALSE,
  ) +

  # Data points
  geom_point(
    shape = 21, fill = "white",
    show.legend = FALSE,
  ) +

  # Error bars
  geom_errorbar(
    data = df_error,
    mapping = aes(y = NULL, ymin = Mean - SE, ymax = Mean + SE),
    width = 0.25,
    show.legend = FALSE,
  ) +

  # Split and repeat the same plot for each BAT
  # https://ggplot2.tidyverse.org/reference/facet_wrap.html
  facet_wrap(
    vars(BAT),
    scales = "free_x",
    strip.position = "bottom",
    labeller = as_labeller(\(names) paste(names, "\nBAT")),
  ) +

  # Colors and labels
  scale_fill_manual(
    name = "",
    labels = labels,
    values = fills,
  ) +
  scale_color_manual(
    name = "",
    labels = labels,
    values = colors,
  ) +

  # Artificial x axis
  annotate(
    geom = "segment", x = -Inf, xend = Inf, y = 150, yend = 150, linewidth = 1
  ) +

  # Coordinate axes
  coord_cartesian(
    ylim = c(150, 300),
  ) +
  # Consider ggbreak::scale_y_break to plot a broken axis
  scale_y_continuous(
    expand = c(0, 0),
  ) +

  # Axis labels
  labs(
    x = "",
    y = "Serum Val (\U03BCM)", # \U03BC = µ
  ) +

  # Design
  theme_bw() +
  theme(
    axis.line.y = element_line(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom",
  ) +
  NULL

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("bars", bars_plot, width = 5, height = 5)
