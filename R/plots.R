source_files <- c(
  list.files(file.path("source"), "*.R", full.names = TRUE),
  list.files(file.path("R", "source"), "*.R", full.names = TRUE)
)

required_pkgs <- c(
  # > grep -v -E -R "^#" R/* | grep -oP "\w+(?=::)" \
  # >   | grep -v -E "BiocManager|devtools" | sort -u \
  # >   | sed 's/^.*$/"\0",/' | tr "\n" " " | sed 's/, $/\n/' | fold -w 78 -s
  "circlize", "ComplexHeatmap", "cowplot", "dplyr", "eulerr", "forcats",
  "ggbeeswarm", "ggplot2", "ggsignif", "ggpubr", "grid", "latex2exp", "stringr",
  "survival", "survminer", "tibble", "tidyr", "tools", "udunits2"
)
required_pkg_vers <- c("ggplot2" = "3.4.0")
bioc_pkgs <- c("ComplexHeatmap")

installed_pkgs <- rownames(installed.packages())

missing_pkgs <- setdiff(required_pkgs, installed_pkgs)

missing_bioc_pkgs <- intersect(missing_pkgs, bioc_pkgs)
if (length(missing_bioc_pkgs) > 0) {
  required_pkgs <- append(required_pkgs, "BiocManager")
}

missing_cran_pkgs <- setdiff(
  required_pkgs, union(rownames(installed.packages()), bioc_pkgs)
)

outdated_cran_pkgs <- c()
for (pkg in names(required_pkg_vers)) {
  if (
    (pkg %in% rownames(installed.packages())) &&
      (packageVersion(pkg) < required_pkg_vers[pkg])
  ) {
    outdated_cran_pkgs <- append(outdated_cran_pkgs, pkg)
  }
}

install_cran_pkgs <- union(missing_cran_pkgs, outdated_cran_pkgs)

tryCatch(
  {
    install.packages(install_cran_pkgs, repos = "https://cran.r-project.org/")
  },
  error = function(cond) {
    # https://github.com/wch/rs/blob/5a6ee8/src/library/utils/R/packages2.R#L330
    .libPaths(c(
      unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep))[1L],
      .libPaths()
    ))
    install.packages(install_cran_pkgs, repos = "https://cran.r-project.org/")
  }
)

if (length(missing_bioc_pkgs) > 0) {
  BiocManager::install(missing_bioc_pkgs)
}

wd <- getwd()
for (source_file in source_files) {
  message("Processing ", source_file, " ...")
  source(source_file, chdir = TRUE)
  message("... done.")
  message()
}
setwd(wd)
