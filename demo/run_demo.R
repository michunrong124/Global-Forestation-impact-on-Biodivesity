# Run from the repository root with Rscript --vanilla demo/run_demo.R.
suppressPackageStartupMessages({
  library(raster)
  library(terra)
})

run_demo <- function() {
  work <- tempfile("forestation-demo-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE))
  grid <- rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4000,
               ymin = 0, ymax = 4000, crs = "+proj=eck4 +datum=WGS84 +units=m")
  layer <- function(values) setValues(grid, values)
  save_input <- function(name, values) {
    writeRaster(layer(values), file.path(work, name))
  }

  # Cells 1–5 are afforested and 6–10 reforested. Cells 11–16 exercise
  # exclusion of unchanged forest, current nonforest, urban, missing PNV,
  # missing forest type, and an excluded last nonforest class.
  origin <- c(55, 22, 55, 33, 66, 44, 44, 44, 22, 44, 44, 55, 11, 22, 55, 55)
  prior <- c(55, 22, 33, 22, 55, 22, 33, 22, 33, 55, 44, 55, 22, 22, 55, 99)
  current <- rep(44, 16)
  current[12] <- 55
  save_input("Landcover1900.tif", origin)
  for (year in c("1920", "1940", "1960_1", "1980_1")) {
    save_input(paste0("Landcover", year, ".tif"), origin)
  }
  # Cell 4 was last nonforested in 1980, rather than 2000.
  earlier <- origin
  earlier[4] <- prior[4]
  writeRaster(layer(earlier), file.path(work, "Landcover1980_1.tif"), overwrite = TRUE)
  latest_prior <- prior
  latest_prior[4] <- 44
  save_input("Landcover2000_1.tif", latest_prior)
  save_input("Landcover2020_1.tif", current)
  save_input("pnv_reclassify_Eckert.tif", replace(rep(NA_real_, 16), c(2, 4, 9), c(55, 55, 44)))
  save_input("Natu_Plant2020_Eckert2.tif", c(1, 2, 3, 6, 6, 1, 2, 5, 6, 6, 1, 1, 1, 1, 7, 1))

  # Evaluate the tracked generator, redirecting its literal input/output paths
  # to the synthetic workspace. Its raster operations execute unchanged.
  generator <- "MainAnalysis/Generate_afforestation_reforestation_rasterfile.R"
  redirect_paths <- function(expr, resolve) {
    if (is.character(expr) && length(expr) == 1L && startsWith(expr, "/")) {
      return(resolve(expr))
    }
    if (is.call(expr)) return(as.call(lapply(as.list(expr), redirect_paths, resolve = resolve)))
    expr
  }
  generation <- new.env()
  for (expr in parse(generator)) {
    eval(redirect_paths(expr, function(path) file.path(work, basename(path))), generation)
  }
  stopifnot(values(generation$last_non_forest_year)[4] == 1980)
  forest_codes <- c(44, 44, 45, 46, 46)
  stopifnot(
    identical(as.vector(values(generation$Landcover2020_affClip2)), c(forest_codes, rep(NA_real_, 11))),
    identical(as.vector(values(generation$Landcover2020_refClip2)), c(rep(NA_real_, 5), forest_codes, rep(NA_real_, 6)))
  )

  habitat <- data.frame(scientificName = c("Demo A", "Demo B", "Demo C"),
                        code = c("55, 44", "22, 45", "33, 46"))
  elevation <- c(100, 600, rep(100, 14))
  range_cells <- list(1:16, setdiff(1:16, 7), 1:16)
  for (i in seq_len(nrow(habitat))) {
    # Demo setup for the precomputed range/elevation mask consumed by AOH.
    eligible <- seq_len(16) %in% range_cells[[i]] & elevation >= 0 &
      elevation <= c(700, 500, 700)[i]
    save_input(paste0(habitat$scientificName[i], ".tif"), ifelse(eligible, 1, NA))
  }

  forest_types <- c(N = "Natural", P = "Plantation", A = "Agroforestry")
  run_aoh <- function(mode, suffix, period) {
    taxon <- if (mode == "Afforestation") "Ampbhian" else "Amphibian"
    script <- file.path("MainAnalysis", "AOH",
                       paste0(taxon, "_", mode, "_DistClip_", period, "_Ex_Alt3Cate.R"))
    env <- list2env(list(input_dir = paste0(work, "/"), Habitat = habitat,
                         land_use = raster(file.path(work, paste0("Landcover", period, "_", suffix, "Clip.tif"))),
                         Fore3 = raster(file.path(work, paste0("Landcover2020_", suffix, "Clip.tif")))))
    for (type in names(forest_types)) {
      path <- file.path(work, mode, period, forest_types[[type]])
      dir.create(path, recursive = TRUE)
      env[[paste0("out_dir", type)]] <- paste0(path, "/")
    }
    # Run the original forest-type preprocessing and worker functions.
    # Bind inputs above and call the workers serially on each synthetic species.
    selected <- c("ForeN", "ForeP", "ForeA", "land_useN", "land_useP", "land_useA",
                  "extract_actual_distribution", paste0("MyBiomodSF", c("N", "P", "A")))
    for (expr in parse(script)) {
      if (is.call(expr) && identical(expr[[1]], as.name("<-"))) {
        lhs <- expr[[2]]
        target <- if (is.name(lhs)) lhs else lhs[[2]]
        if (as.character(target) %in% selected) eval(expr, env)
      }
    }
    for (type in names(forest_types)) {
      worker <- env[[paste0("MyBiomodSF", type)]]
      for (i in seq_len(nrow(habitat))) worker(i)
    }
  }

  counts <- list()
  for (mode in c("Afforestation", "Reforestation")) {
    suffix <- if (mode == "Afforestation") "aff" else "ref"
    for (period in c("Mid", "2020")) run_aoh(mode, suffix, period)
    for (forest_type in unname(forest_types)) {
      # Reuse the amphibian area worker for each mode/type. Its result
      # counts 1 km² cells, so the reported counts are also areas in km².
      script <- file.path("MainAnalysis", paste0(mode, "_SpeciesArea_Statistic_", forest_type, ".R"))
      env <- list2env(list(AmpList2 = habitat["scientificName"]))
      resolve <- function(path) {
        parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
        period <- tail(parts, 3L)[1L]
        if (!period %in% c("Mid", "2020")) stop("Unexpected area worker path: ", path)
        paste0(file.path(work, mode, period, forest_type), "/")
      }
      for (expr in parse(script)) {
        if (is.call(expr) && identical(expr[[2L]], quote(Afun))) {
          eval(redirect_paths(expr, resolve), env)
        }
      }
      for (i in seq_len(nrow(habitat))) {
        area <- env$Afun(i)
        counts[[length(counts) + 1L]] <- data.frame(
          mode = mode, forest_type = forest_type, species = area$Species,
          loss_km2 = area$YMid, gain_km2 = area$Y2020, retained_km2 = area$Overlap
        )
      }
    }
  }

  result <- do.call(rbind, counts)
  expected <- read.csv("demo/expected_aoh.csv")
  stopifnot(isTRUE(all.equal(result, expected)))
  output <- commandArgs(trailingOnly = TRUE)
  if (length(output) > 1L) stop("Supply at most one output CSV path.")
  if (length(output) == 0L) output <- "demo/aoh_results.csv"
  write.csv(result, output, row.names = FALSE)
  cat("Verified", nrow(result), "AOH rows in", output, "\n")
}

run_demo()
