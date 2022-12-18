# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data obtained from the authors of
# https://doi.org/10.1126/scitranslmed.aaz2841, Fig. 6D
# (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7982985/figure/F6/)
# Optimized for file size by omitting unnecessary rows and columns
df_data <- read.csv(file.path("heatmap", "data_min.csv"))

# Names of genes to plot
genes <- c(
  "Acot3", "Acsl1", "Acot4", "Ppargc1a", "Ppara", "Hadhb", "Acbd5", "Hadha",
  "Acaa2", "Nudt19", "Acadm", "Acadl", "Cpt2", "Acadvl", "Acadsb", "Acads",
  "Acox1", "Acsm5", "Timp1", "Col1a1", "Ccr2", "Col1a2", "Serpine1", "Ccl2",
  "Tnfrsf9", "Ccr1", "Tgfbr1", "Nfkb1", "Tnfrsf1a", "Tlr4", "Tgfbr2", "Cd80",
  "Nfkb2", "Cd86", "Tgfb1", "Tnf", "Cxcl1", "Relb", "Tnfrsf12a", "Col4a2",
  "Col12a1", "Ccr5", "Icam1", "Tgfb2", "Tlr1", "Col4a1", "Ccl5", "Tlr2",
  "Irak3", "Tgfb3"
)

# Function annotation for the above list of genes
fib <- "Fibrosis"
inf <- "Inflammation"
fat <- "Fatty acid degradation"
func <- factor(c(
  rep(fat, 18), rep(fib, 2), rep(inf, 1), rep(fib, 2), rep(inf, 3), rep(fib, 1),
  rep(inf, 3), rep(fib, 1), rep(inf, 3), rep(fib, 1), rep(inf, 4), rep(fib, 2),
  rep(inf, 2), rep(fib, 1), rep(inf, 1), rep(fib, 1), rep(inf, 3), rep(fib, 1)
), levels = c(fib, inf, fat))
names(func) <- genes

# Add reference column
df_data <- tibble::add_column(
  df_data,
  CD_CD_log2FoldChange = 0, .after = "Leu_CD_log2FoldChange"
)

# Column names
groups <- c(
  NASH = "NASH",
  Leu = "Leu",
  CD = "CD",
  Gly = "Gly",
  DT125 = "DT-109 0.125",
  DT500 = "DT-109 0.5"
)
columns <- paste0(names(groups), "_CD_log2FoldChange")

# Select required data (redundant here since we are working with data_min.csv)
df_data <- df_data[df_data$mgi_symbol %in% genes, c("mgi_symbol", columns)]
stopifnot(length(setdiff(genes, df_data$mgi_symbol)) == 0)

# Set row and column names
rownames(df_data) <- df_data$mgi_symbol
df_data$mgi_symbol <- NULL
colnames(df_data) <- groups

# Sort function annotations by dataframe order
func <- func[order(match(names(func), rownames(df_data)))]

# Convert to matrix, compute z scores per row
data <- as.matrix(df_data)
data <- t(scale(t(data)))

# Colors
col_fun <- circlize::colorRamp2(
  c(-1.75, seq(-1.5, 1.5, length.out = 7), +1.75),
  c(
    "#2d5aa0",
    "#4f7db5", "#96c1dc", "#def2ee", "#feffb3", "#fce186", "#f99657", "#e14630",
    "#ca1216"
  ),
)

# Suppress some output by invoking a nop command
suppressPackageStartupMessages(invisible(ComplexHeatmap::gt_render("")))

# Function annotation by colors
ha_cols <- c("blueviolet", "red", "blue")
names(ha_cols) <- levels(func)
ha <- ComplexHeatmap::rowAnnotation(
  Function = func, col = list(Function = ha_cols),
  gp = grid::gpar(col = "grey", lwd = 1),
  annotation_label = ComplexHeatmap::gt_render("**Function**"),
  annotation_name_rot = -90
)

# Generate Heatmap plot object
# https://jokergoo.github.io/ComplexHeatmap-reference/
heatmap_plot <- ComplexHeatmap::Heatmap(
  data,
  name = " ",
  col = col_fun,
  row_dend_reorder = FALSE,
  column_dend_reorder = FALSE,
  left_annotation = ha,
  row_labels = ComplexHeatmap::gt_render(
    paste0("*", rownames(df_data), "*")
  ),
  row_names_side = "right",
  row_dend_side = "left",
  column_names_rot = -90,
  rect_gp = grid::gpar(col = "grey", lwd = 1),
  heatmap_legend_param = list(direction = "horizontal"),
)

# Wrap heatmap object in a plot function to use below helper functions
plot_fun <- function() {
  ComplexHeatmap::draw(
    heatmap_plot,
    heatmap_legend_side = "top",
    annotation_legend_side = "top",
  )
}

# Use helper function from helpers.R to store PDF and PNG files at defined size
save_cairo_pdf_and_png("heatmap", plot_fun, width = 3, height = 12)
