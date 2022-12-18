# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data obtained from the authors of
# https://doi.org/10.1016/j.cell.2019.08.010, Fig. S2C
# (https://www.cell.com/cell/fulltext/S0092-8674(19)30898-0#figs2)
df_data <- read.csv(file.path("violin_swarm", "data.csv"))

# Convert (Day0, Day7, Day30) value columns into (Day, Value) columns
# https://tidyr.tidyverse.org/reference/pivot_longer.html
df_data <- tidyr::pivot_longer(
  df_data,
  cols = tidyr::starts_with("Day"),
  names_to = "Day",
  values_to = "Value",
  names_transform = list(
    Day = \(names) as.factor(as.numeric(sub("Day", "", names)))
  ),
)

# Rename groups and set order
df_data$Group[df_data$Group == "Control"] <- "Controls"
df_data$Group[df_data$Group == "Abx"] <- "Abx-treated"
df_data$Group <- factor(df_data$Group, levels = c("Controls", "Abx-treated"))

# Format phase names
df_data$Phase <- paste0("A/California (phase ", df_data$Phase, ")")

# Fine-tune quantiles to draw and dodge width between groups
q <- 0.85
dw <- 0.9

# Generate ggplot2 plot object
# https://ggplot2.tidyverse.org/

# Configure dataframe (columns) and mapping (from columns to aesthetics)
violin_plot <- ggplot(
  data = df_data,
  mapping = aes(x = Day, y = Value, color = Group, fill = Group)
) +

  # Dotted quantiles (NB: drawn based on density estimates)
  # https://github.com/tidyverse/ggplot2/issues/4120
  geom_violin(
    draw_quantiles = c(q, 1 - q),
    linetype = "dotted",
    fill = NA,
    linewidth = 0.25,
    position = position_dodge(width = dw),
    scale = "width",
    width = 0.5,
    adjust = 2 / 3,
  ) +

  # Violins
  geom_violin(
    alpha = 0.125,
    color = "white",
    position = position_dodge(width = dw),
    scale = "width",
    width = 0.5,
    adjust = 2 / 3,
  ) +

  # Medians
  # https://ggplot2.tidyverse.org/reference/stat_summary.html
  stat_summary(
    geom = GeomHpline,
    width = 0.24,
    fun = median,
    linewidth = 1,
    position = position_dodge(width = dw),
    show.legend = FALSE,
  ) +

  # Swarm plots
  # https://github.com/eclarke/ggbeeswarm#readme
  ggbeeswarm::geom_beeswarm(
    size = 1.5,
    cex = 3.5,
    shape = 21,
    fill = "white",
    dodge.width = dw,
    show.legend = FALSE,
  ) +

  # Fix vertical axis limits, making sure the 0.30 label is printed
  coord_cartesian(
    ylim = c(0.1, 0.3000000000000001),
    expand = FALSE,
  ) +

  # Colors
  scale_color_manual(
    values = c("#003399", "red"),
  ) +
  scale_fill_manual(
    values = c("#003399", "red"),
  ) +

  # Split and repeat the same plot for each phase
  # https://ggplot2.tidyverse.org/reference/facet_wrap.html
  facet_wrap(
    ~Phase,
    # "free_y" to print vertical axis labels next to each subplot
    scales = "free_y",
  ) +

  # Axis labels
  labs(
    x = "days",
    y = "HA-specific IgG2 (OD)",
  ) +

  # Design
  guides(
    color = guide_legend(title = ""),
    fill = guide_legend(title = ""),
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "vertical",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(size = 11),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 11),
    axis.title.x = element_text(vjust = 7, hjust = 1),
  ) +
  NULL

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("violin_swarm", violin_plot, width = 7, height = 4)
