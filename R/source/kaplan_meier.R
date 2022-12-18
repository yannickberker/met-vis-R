# Setup: change working directory and include common helper functions
if (dir.exists(source_dir <- file.path("R", "source"))) setwd(source_dir)
source(file.path("..", "helpers.R"))

# Raw plot data converted from https://doi.org/10.1038/nature23876,
# Source data to Fig. 5 (https://www.nature.com/articles/nature23876#Sec27),
# File "41586_2017_BFnature23876_MOESM12_ESM.xlsx", Sheet "5b"
df_data <- read.csv(file.path("kaplan_meier", "data.csv"))

# Fit a survival curve (Simple Kaplan-Meier)
# https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf
fit <- survival::survfit(survival::Surv(Days, Status) ~ Group, data = df_data)

# Remove "Group=" from names
# https://stringr.tidyverse.org/reference/str_replace.html
names(fit$strata) <- stringr::str_replace(names(fit$strata), "Group=", "")

# Locations of number annotations based on values from
# unique(sort(df_data$Days)) and unique(sort(fit$surv))
df_numbers <- read.csv(file.path("kaplan_meier", "number_loc.csv"))
df_numbers <- merge(df_numbers, table(df_data$Group), by.x = "Group", by.y = 1)

# Location of significance brackets
df_signif <- read.csv(file.path("kaplan_meier", "signif_loc.csv"))

# Draw survival curves as a ggplot2 plot object
# https://rpkgs.datanovia.com/survminer/
surv_plot <- survminer::ggsurvplot(
  fit,
  data = df_data, surv.scale = "percent", xlab = "Days",
  # Fine-tuning (1.1) to prevent cutting off annotations near the top and right
  axes.offset = FALSE, xlim = c(0, 300 * 1.1), ylim = c(0, 1 * 1.1),
  size = 0.5, censor.shape = "|", censor.size = 3,
)$plot +

  # Colors
  scale_color_manual(
    # This may crash on R v4.1 on Windows - use R v4.2 in that case
    labels = make_tex_labels(df_numbers$Group),
    values = c("#000000", "#394c7c", "#b42b1e", "#781b45", "#75c0cf"),
  ) +

  # Dashed vertical line at 6 weeks
  geom_vline(xintercept = 42, linetype = "dashed") +

  # Text annotations, fine-tune w.r.t. vertical line (0.04, 0.35)
  annotate(
    "text",
    x = 42, y = 0.04, hjust = 0.35,
    label = "Low  \U2192  High asc.", # \U2192 = <U+2192>
  ) +

  # Curve annotations
  geom_text(
    data = df_numbers,
    aes(label = paste("n =", Freq), x = x, y = y, hjust = hjust, color = Group),
    # Vertical text shift up by one line height to avoid censor shape
    vjust = -1,
    # Fine-tune horizontal text shift in units of days (5.0)
    nudge_x = 5.0 * (1 - 2 * df_numbers$hjust),
    # Remove "a" from legend
    show.legend = FALSE,
  ) +

  # Significance brackets, fine-tuned (5)
  geom_segment(
    aes(x = Day - 5, xend = Day, y = Top, yend = Top),
    data = df_signif,
  ) +
  geom_segment(
    aes(x = Day, xend = Day, y = Top, yend = Bottom),
    data = df_signif,
  ) +
  geom_segment(
    aes(x = Day - 5, xend = Day, y = Bottom, yend = Bottom),
    data = df_signif,
  ) +

  # Significance bracket labels, fine-tuned (10)
  geom_text(
    aes(x = Day + 10, y = (Top + Bottom) / 2, label = Level, angle = -90),
    data = df_signif,
  ) +

  # Design
  guides(
    color = guide_legend(title = NULL, direction = "vertical"),
  ) +
  theme(
    legend.text.align = 0,
  ) +

  NULL

# Use helper function from helpers.R to store PDF and PNG files at defined size
ggsave_cairo_pdf_and_png("kaplan_meier", surv_plot, width = 5, height = 5)
