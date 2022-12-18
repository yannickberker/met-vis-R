# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Include three existing plots (unmodified), each storing a ..._plot variable
source("swarm.R")
source("bars.R")
source("volcano.R")

# Arrange all three plots in a grid (nrow = 1 <=> single row)
# https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html
grid_plot <- ggpubr::ggarrange(
  plotlist = list(swarm_plot, bars_plot, volcano_plot),
  nrow = 1,
  widths = c(2, 1.5, 2),
  labels = "auto"
)

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("grid", grid_plot, width = 12, height = 4)
