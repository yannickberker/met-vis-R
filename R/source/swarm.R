# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data obtained from the authors of
# https://doi.org/10.1016/j.cell.2019.08.010, Fig. S3E
# (https://www.cell.com/cell/fulltext/S0092-8674(19)30898-0#figs3)
df_data <- read.csv(file.path("swarm", "data.csv"))

# Convert (LC_Control, LC_Abx, HC_Control, HC_Abx) value columns into
# (Chain, Group, Value) columns
# https://tidyr.tidyverse.org/reference/pivot_longer.html
df_data <- tidyr::pivot_longer(
  df_data,
  cols = tidyr::everything(),
  names_to = c("Chain", "Group"),
  values_to = "Value",
  names_sep = "_",
)

# Omit incomplete entries
df_data <- df_data[complete.cases(df_data), ]

# Rename entries
df_data$Chain <- paste("# of", df_data$Chain, "Mutations")
df_data$Group[df_data$Group == "Control"] <- "Controls"

# Display factors in reverse alphabetical order
df_data$Chain <- factor(
  df_data$Chain,
  levels = rev(sort(unique(df_data$Chain)))
)
df_data$Group <- factor(
  df_data$Group,
  levels = rev(sort(unique(df_data$Group)))
)

# Data points to plot invisibly for per-facet y-axis limits, fine-tuned (30/50)
df_ylim <- data.frame(
  Chain = paste("# of", c("LC", "LC", "HC", "HC"), "Mutations"),
  Group = "Controls",
  Value = c(0, 30, 0, 50)
)
df_ylim$Chain <- factor(df_ylim$Chain, levels = levels(df_data$Chain))
df_ylim$Group <- factor(df_ylim$Group, levels = levels(df_data$Group))

# In-facet text labels, fine-tuned (30/50)
df_text <- data.frame(
  Chain = rep(levels(df_data$Chain), 2),
  Group = rep(levels(df_data$Group), each = 2),
  Value = rep(c(30, 50), 2)
)
df_text$Chain <- factor(df_text$Chain, levels = levels(df_data$Chain))
df_text$Group <- factor(df_text$Group, levels = levels(df_data$Group))

# Significance brackets, fine-tuned (30/50)
df_signif <- data.frame(
  Chain = levels(df_data$Chain),
  xmin = levels(df_data$Group)[1],
  xmax = levels(df_data$Group)[2],
  y_position = c(30, 50),
  annotations = "NS"
)
df_signif$Chain <- factor(df_signif$Chain, levels = levels(df_data$Chain))

# Colors
grey <- "#857f7e"
blue <- "#649cfc"
red <- "#ff0307"

# (Lacking information on how to identify dark-blue squares)

# Generate ggplot2 plot object
# https://ggplot2.tidyverse.org/

# Configure dataframe (columns) and mapping (from columns to aesthetics)
swarm_plot <- ggplot(
  data = df_data,
  mapping = aes(x = Group, y = Value, color = Group)
) +

  # Swarm plots
  # https://github.com/eclarke/ggbeeswarm#readme
  ggbeeswarm::geom_beeswarm(
    mapping = aes(shape = Group),
    size = 1,
    cex = 2,
  ) +

  # In-facet text labels
  geom_text(
    data = df_text,
    mapping = aes(x = Group, y = Value * 0.95, label = Group),
  ) +

  # Significance brackets
  # https://cran.r-project.org/web/packages/ggsignif/vignettes/intro.html
  suppress_warning_signif(
    ggsignif::geom_signif(
      data = df_signif,
      mapping = aes(
        color = NULL,
        xmin = xmin,
        xmax = xmax,
        y_position = y_position * 0.9,
        annotations = annotations,
      ),
      manual = TRUE,
      show.legend = FALSE,
    )
  ) +

  # Medians
  # https://ggplot2.tidyverse.org/reference/stat_summary.html
  stat_summary(
    geom = GeomHpline,
    fun = median,
    color = grey,
    linewidth = 1,
    width = 0.15,
  ) +

  # Error bars
  # https://ggplot2.tidyverse.org/reference/stat_summary.html
  stat_summary(
    geom = "errorbar",
    color = grey,
    linewidth = 1,
    width = 0.25,
    fun.data = \(x, mult = 1) {
      m <- median(x)
      q_low <- quantile(x, 0.25)
      q_high <- quantile(x, 0.75)
      data.frame(
        ymin = m + mult * (q_low - m),
        ymax = m + mult * (q_high - m),
        y = m
      )
    },
  ) +

  # Invisible (alpha = 0) data points to set per-facet vertical axis limits
  geom_point(
    data = df_ylim,
    alpha = 0,
  ) +

  # Horizontal axis limits
  coord_cartesian(
    xlim = c(0.5, 2.5),
    expand = FALSE,
  ) +

  # Manual colors, fills, and shapes
  scale_color_manual(
    values = c(blue, red),
  ) +
  scale_fill_manual(
    values = c(blue, red),
  ) +
  scale_shape_manual(
    values = c(15, 16), # 15 = filled square, 16 = filled circle
  ) +

  # Split and repeat the same plot for each phase
  # https://ggplot2.tidyverse.org/reference/facet_wrap.html
  facet_wrap(
    ~Chain,
    scales = "free_y",
    strip.position = "left",
  ) +

  # Design
  guides(
    color = guide_legend(title = ""),
    fill = guide_legend(title = ""),
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 11),
    axis.text = element_text(color = "black", size = 11),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
  ) +
  NULL

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("swarm", swarm_plot, width = 7, height = 3)
