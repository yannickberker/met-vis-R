library(ggplot2)

# Make plots as reproducible as possible
set.seed(0)

#' Reformat a few labels for nicer output
#'
#' @param labels Text labes to reformat.
make_tex_labels <- function(labels) {
  labels <- gsub(
    "Tet2<U+2206>/\\+;Flt3ITD/\\+", "$Tet2^{\\Delta/+};Flt3^{ITD/+}$", labels
  )
  labels <- gsub(
    "Gulo\\-/\\-", "$Gulo^{-/-}$", labels
  )
  labels <- gsub(
    "P = (.*)", "$\\\\textit{P} = \\1$", labels
  )
  unname(latex2exp::TeX(labels))
}

#' From https://github.com/wilkelab/ungeviz/blob/aeae12b/R/geom_hpline.R#L48-L68
# nolint start: object_name_linter.
GeomHpline <- ggproto("GeomHpline", GeomSegment,
  required_aes = c("x", "y"),
  non_missing_aes = c("linewidth", "colour", "linetype", "width"),
  default_aes = aes(
    colour = "black", linewidth = 1, linetype = 1, alpha = NA, width = 0.5
  ),
  draw_panel = function(self, data, panel_params, coord,
                        arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data <- dplyr::mutate(data, x = x - width / 2, xend = x + width, yend = y)
    ggproto_parent(GeomSegment, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)
# nolint end

#' Suppress specific warning
#'
#' @warning_message_parts Parts of warning message to suppress
#' @expression Expression to evaluate
suppress_warning <- function(warning_message_parts, expression) {
  withCallingHandlers(expression, warning = function(w) {
    if (all(sapply(warning_message_parts, function(p) grepl(p, w)))) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Suppress warning for ggsignif::geom_signif
#'
#' @expression Expression to evaluate
suppress_warning_signif <- function(expression) {
  warnings_message_parts <- c(
    "Ignoring unknown aesthetics", "xmin", "xmax", "y_position", "annotations"
  )
  suppress_warning(warnings_message_parts, expression)
}

#' Call [ggplot2::ggsave] using the Cairo pdf and png devices, respectively.
#'
#' @param filename Name of file to create in "output" folder.
#'   Any extension will be stripped before appending ".pdf" and ".png".
#' @param ... Other arguments passed on to [ggplot2::ggsave].
ggsave_cairo_pdf_and_png <- function(filename, ...) {
  stem <- filestem(filename)

  ggplot2::ggsave(paste0(stem, ".pdf"), ..., device = cairo_pdf)
  ggplot2::ggsave(paste0(stem, ".png"), ..., type = "cairo")
}

#' Same as [ggsave_cairo_pdf_and_png] for non-ggplot2 plots.
#'
#' @param filename See [ggplot2::ggsave_cairo_pdf_and_png].
#' @param plot_fun Function returning plot, taking no argument.
#' @param path,scale,width,height,units,dpi See [ggplot2::ggsave].
save_cairo_pdf_and_png <- function(filename,
                                   plot_fun,
                                   path = ".",
                                   scale = 1,
                                   width = 7,
                                   height = 7,
                                   unit = c("in", "cm", "mm", "px"),
                                   dpi = 300) {
  stem <- filestem(filename)

  unit <- match.arg(unit)
  dims_in <- tryCatch(
    {
      # try ggplot2 private function first due to its accuracy, see
      # > ggplot2:::plot_dim(1, units="in") - 1
      dim <- ggplot2:::plot_dim(
        dim = c(width, height),
        scale = scale,
        units = unit,
        dpi = dpi,
      )
      list(width = dim[1], height = dim[2])
    },
    error = function(cond) {
      # fall back to udunits2, which is less accurate, compare
      # > udunits2::ud.convert(1, "in", "in") - 1
      as.list(udunits2::ud.convert(
        scale * c(width = width, height = height), unit, "in"
      ))
    }
  )

  # Save PDF
  cairo_pdf(
    filename = file.path(path, paste0(stem, ".pdf")),
    width = dims_in$width,
    height = dims_in$height,
    fallback_resolution = dpi,
  )
  show(plot_fun())
  dev.off()

  # Save PNG
  png(
    type = "cairo",
    filename = file.path(path, paste0(stem, ".png")),
    width = scale * width,
    height = scale * height,
    units = unit,
    res = dpi,
  )
  show(plot_fun())
  dev.off()

  # Return nothing
  invisible()
}

#' Get output prefix for filename. Strips extension and prepends "../output".
#'
#' @param filename The input filename.
filestem <- function(filename) {
  folder <- file.path("..", "output")
  if (!dir.exists(folder)) dir.create(folder)

  stem <- tools::file_path_sans_ext(filename)
  file.path(folder, stem)
}
