# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data obtained from the authors of
# https://doi.org/10.1038/s41586-021-04025-w, Fig. 1C
# (https://www.nature.com/articles/s41586-021-04025-w#Fig1)
df_data <- read.csv(file.path("volcano", "data.csv"))

# Randomize order of rows to avoid biased plots
df_data <- df_data[sample(nrow(df_data)), ]

# Filter samples to plot in green, using regular expressions ("|" <=> OR)
# https://stringr.tidyverse.org/reference/str_detect.html
samples_green <- c("ALAS1", "HMOX1", "SLC25A39")
re <- paste(samples_green, collapse = "|")
df_green <- df_data[
  stringr::str_detect(df_data$Description, re) & df_data$p < 0.01,
]

# (Lacking information on how to filter "iron-sulfur cluster" and "translation")

# Names of samples to annotate
samples_annot <- c("ALAS1", "HMOX1", "SLC25A39", "SDHB", "CISD3")
# Filter samples with description that contain the above, but not "Fragment";
# ensure resulting dataframe is in the same order as `samples_annot`
# https://stringr.tidyverse.org/reference/str_detect.html
re <- paste(samples_annot, collapse = "|")
df_annot <- do.call(rbind, lapply(
  samples_annot,
  \(s) df_data[stringr::str_detect(df_data$Description, s) &
    !stringr::str_detect(df_data$Description, "Fragment"), ]
))
df_annot$label_text <- samples_annot

# Fine-tune label position in x and y, and text alignment
df_annot$label_log2_diff <- df_annot$log2_diff + c(-2, 3.6, -2, -2, -2) / 3
df_annot$label_p <- df_annot$p * c(0.2, 1, 5, 0.2, 1)
df_annot$hjust <- c(1, 0.5, 0.5, 1, 1)

# Define colors
grey <- "#dedddc"
darkgrey <- "#817d7c"
violet <- "#d0498e"
blue <- "#0095df"
green <- "#39a935"

# Generate ggplot2 plot object
# https://ggplot2.tidyverse.org/

# Configure dataframe (columns) and mapping (from columns to aesthetics)
volcano_plot <- ggplot(
  data = df_data,
  mapping = aes(x = log2_diff, y = -log10(p)),
) +

  # Line at P = 0.01
  geom_segment(
    aes(x = -Inf, xend = Inf, y = -log10(0.01), yend = -log10(0.01)),
    linewidth = 0.25,
    linetype = 2,
  ) +

  # Annotation lines
  geom_segment(
    data = df_annot,
    mapping = aes(xend = label_log2_diff, yend = -log10(label_p)),
  ) +

  # Annotation labels
  geom_label(
    data = df_annot,
    mapping = aes(
      x = label_log2_diff,
      y = -log10(label_p),
      hjust = hjust,
      label = label_text,
    ),
    vjust = 0.5,
    label.size = 0,
  ) +

  # Data points in grey ...
  geom_point(
    shape = 21,
    colour = darkgrey,
    fill = grey,
    size = 4,
    stroke = 1,
  ) +

  # ... and green
  geom_point(
    data = df_green,
    shape = 21,
    colour = darkgrey,
    fill = green,
    size = 4,
    stroke = 1,
  ) +

  # Consider ggh4x::coord_axes_inside() to center plot on vertical axis
  coord_cartesian(
    xlim = c(-4, 4),
    ylim = c(0, 8),
    expand = FALSE,
  ) +

  # Axis labels
  labs(
    x = expression(
      "log"[2] * "(fold change in protein abundance BSO/untreated)"
    ),
    y = expression(
      -"log(" * italic(P) * " value)"
    ),
  ) +

  # P = 0.01 label
  annotate(
    "text",
    x = 4,
    y = -log10(0.01),
    hjust = 1.1,
    vjust = -0.5,
    label = list(substitute(paste(italic("P"), " = 0.01"))),
    parse = TRUE,
  ) +

  # Design
  theme_bw() +
  theme(
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.ticks.length = unit(0.3, "cm"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  NULL

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("volcano", volcano_plot, width = 5, height = 5)
