# Install explicitly into the first R library. Use R_LIBS_USER for isolation.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !args %in% c("demo", "analysis")) {
  stop("Usage: Rscript --vanilla install_packages.R demo|analysis", call. = FALSE)
}

packages <- c("raster", "terra")
if (args == "analysis") {
  packages <- sort(c(packages, "data.table", "dplyr", "ggplot2",
                     "gridExtra", "htmlwidgets", "magick", "openxlsx", "plotly",
                     "reshape2", "rworldmap", "rworldxtra", "sf", "snowfall",
                     "stringr", "tidyr", "tidyverse", "webshot"))
}

lib <- .libPaths()[1L]
missing <- setdiff(packages, rownames(installed.packages(lib.loc = lib)))
if (length(missing)) {
  install.packages(missing, lib = lib, repos = "https://cloud.r-project.org")
}
remaining <- setdiff(packages, rownames(installed.packages(lib.loc = lib)))
if (length(remaining)) {
  stop("Packages missing from ", lib, ": ", paste(remaining, collapse = ", "), call. = FALSE)
}
print(installed.packages(lib.loc = lib)[packages, c("Package", "Version"), drop = FALSE])
