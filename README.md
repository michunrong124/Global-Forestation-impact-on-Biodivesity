# Global impacts of afforestation and reforestation on biodiversity

## Overview

Chunrong Mi, Alex Wiebe, Tong Mu, Dan Liang, and David S. Wilcove

This repository contains R scripts for a global study of how afforestation and reforestation changed the modeled area of habitat (AOH) of amphibians, reptiles, birds, and mammals. The study covers 1899–2020, including a HILDA+ update for 1960–2020. The raster generator reads precomputed snapshots for the 1900 baseline and 20-year intervals through 2020. The submission Methods describe each snapshot as the majority class in a five-year window. The snapshot preparation still needs to be verified. It distinguishes afforestation from reforestation and groups 2020 forest cover into natural forest, plantation forest, and agroforestry.

The October 27, 2026 Nature Sustainability submission manuscript describes the workflow in Methods under “Land Use and Forestation Classification,” “Forest Type Classification,” “Species’ Area of Habitat (AOH) and Habitat Mapping,” “Quantifying Biodiversity Impacts,” and “Statistical Analysis.” Its “Code availability” section points to the public repository. The [workflow audit](docs/workflow-audit.md) maps the tracked scripts to the draft figures and identifies missing inputs and handoffs.

## System requirements

**Demo.** R plus the CRAN packages `raster` and `terra`. The demo runs serially and creates its own 4 × 4 grid. It was tested on macOS 26.6.2 (Apple silicon) with R 4.5.1, `raster` 3.6-32, and `terra` 1.9-60. No cluster or third-party input data are required. See [verification](docs/submission-checklist.md) for the measured environment and versions.

**Global analysis.** The submission Methods specify R 4.3.2 for the McNemar tests. The historical README separately reports R 4.3.2 on Princeton University’s HPC cluster, more than 1,000 GB RAM for some scripts, and more than 100 hours of cumulative runtime across species. The exact allocation of analysis stages between these R environments remains unverified. The historical package versions, cluster OS, and basis of the reported memory figure remain to be confirmed. The AOH scripts request 10 or 40 workers via `snowfall`. The statistics scripts also use `parallel::mclapply` with multiple cores, which requires fork support as written. Plan for storage for per-species rasters, intermediate tables, and raster temporary files.

The [dependency inventory](docs/script-dependencies.tsv) lists each original script’s package calls, including worker imports. `parallel` and `stats` ship with R. Source builds of `terra` and `sf` need a compatible toolchain and GDAL, GEOS, PROJ, and SQLite. Check the package requirements for `terra` (C++17 and TBB), `sf` dependencies (including UDUNITS-2 for `units`), and `magick` (ImageMagick++). Fig. 2’s self-contained HTML and PDF export uses Pandoc and PhantomJS, respectively. [terra requirements](https://cran.r-project.org/package=terra) · [sf requirements](https://cran.r-project.org/package=sf) · [HTML widget export documentation](https://rstudio.github.io/bookdown/html-widgets.html)

## Installation guide

Install R and obtain a checkout of this repository. From its root, run the following commands in a POSIX shell. The dedicated R library stays outside the checkout.

```sh
export R_LIBS_USER="$HOME/forestation-r-library"
mkdir -p "$R_LIBS_USER"
Rscript --vanilla install_packages.R demo
```

For the original scripts, use `Rscript --vanilla install_packages.R analysis` instead. The installer lists the resulting versions and uses the platform’s default CRAN package type. Use a separate library for each environment you want to retain. On platforms that compile packages from source, install the system libraries above first. Check `rmarkdown::pandoc_available()` and `webshot::is_phantomjs_installed()` before running the Fig. 2 export script.

The observed binary installation took **4.24–4.40 seconds** for the demo packages and **8.64 seconds** for the analysis package set in empty target libraries on the test Mac. R and some dependencies were already installed in the shared R library. Download speed and source builds can change these times. Setup time for the full HPC workflow remains unmeasured.

## Data availability

The repository includes [the habitat-to-land-cover crosswalk](supplementaryTable/Crosswalk.csv) and [an archive with the 2020 afforestation and reforestation GeoTIFFs](rasterfile.zip). The TIFFs are 1 km × 1 km in Eckert IV, with 2020 forest-type codes 44 (natural), 45 (plantation), and 46 (agroforestry). The archive’s derived-data redistribution terms still need author confirmation. The demo generates its inputs and can run offline after installation.

| Input | Source |
| --- | --- |
| Historical land cover | Winkler, Fuchs, Rounsevell & Herold (2021), *Nature Communications* 12, 2501. The Methods draft also cites a [1960–2020 update](https://bwsyncandshare.kit.edu/s/H2iQG6nMPTaxpqR). [HILDA+ paper](https://doi.org/10.1038/s41467-021-22702-2) · [Public v2.0 archive](https://doi.pangaea.de/10.1594/PANGAEA.974335) |
| Potential natural vegetation | Hengl et al. (2018), *PeerJ* 6, e5457. [PNV paper](https://doi.org/10.7717/peerj.5457) |
| Forest types | Xu et al. (2024), “Changes in the Fine Composition of Global Forests from 2001 to 2020,” *Journal of Remote Sensing* 4. [Forest management dataset](https://zenodo.org/records/10478678) |
| Bird distributions | [BirdLife International Data Zone](http://datazone.birdlife.org/) |
| Amphibian, reptile, and mammal distributions | [IUCN Red List spatial data](https://www.iucnredlist.org/resources/spatial-data-download) |
| Habitat, elevation preferences, and assessments | [IUCN Red List](https://www.iucnredlist.org/) |
| Continent boundaries | [Esri World Continents](https://hub.arcgis.com/datasets/esri::world-continents/about) |
| Elevation | [WorldClim 2.1](https://www.worldclim.org/data/worldclim21.html) |
| Historical context for Fig. S1 | [HYDE 3.3](https://public.yoda.uu.nl/geo/UU01/67UHB4.html) |

The public HILDA+ v2.0 and Xu archives list CC BY 4.0. Confirm the exact releases and preprocessing used for the manuscript before staging inputs. Obtain species range data under the providers’ terms. WorldClim permits academic use and requires permission to redistribute its data. [HILDA+ v2.0 archive](https://doi.pangaea.de/10.1594/PANGAEA.974335) · [Xu archive](https://zenodo.org/records/10478678) · [BirdLife terms](https://datazone.birdlife.org/terms-and-conditions) · [IUCN spatial data terms](https://nc.iucnredlist.org/redlist/content/attachment_files/RLSpatial_metadata_v6.2.pdf) · [WorldClim terms](https://worldclim.org/about.html)

## Demo

From the repository root, using the same R library as for installation, run:

```sh
Rscript --vanilla demo/run_demo.R
```

The script prints `Verified 18 AOH rows in demo/aoh_results.csv` and writes a CSV matching [the expected output](demo/expected_aoh.csv). An optional argument sets a different CSV path in an existing directory. The script checks the forestation masks and all three AOH measures, then writes the CSV. Intermediate rasters are created in a temporary directory. Observed runtime was **8–10 seconds** on the tested Mac.

The example generates synthetic historical, potential-vegetation, and forest-type layers, plus three fictional species. It builds range/elevation masks as fixture inputs and executes the original forestation generator and amphibian AOH and area workers with local paths. The output reports pre-forestation-only habitat (loss), 2020-only habitat (gain), and shared habitat (retained) for each species, mode, and forest type. One cell is 1 km². For example, `Demo A` gains 1 km² and retains 1 km² in naturally afforested cells. The sample ends at AOH accounting, before significance tests and figure creation.

## Instructions for use

1. Stage your data outside the code checkout. For forestation classification, prepare aligned categorical rasters for 1900, 1920, 1940, 1960, 1980, 2000, and 2020, a reclassified potential natural vegetation raster, and a 2020 Xu forest-type raster. Use the original categorical codes and a common Eckert IV grid with 1,000 m cells. Review the generator and Methods before adapting raw products.
2. For AOH, prepare a per-taxon habitat CSV with `scientificName` and `code` (integers separated by comma and space). Provide per-species GeoTIFFs named `<scientificName>.tif` using the same species names. Eligible range/elevation cells must have values and excluded cells must be `NA`. The original AOH scripts consume these precomputed masks. The Mid and 2020 forestation layers must align because the area scripts compare cells directly.
3. In a local working copy of the scripts, replace the input and output roots with paths to your staged data. Use [the path inventory](docs/script-paths.tsv) to locate each binding, including paths assembled by `paste0`. Create the output directories and keep output paths separate from inputs. The original scripts contain overwriting writes and some start by clearing the R session.
4. Run each stage in a fresh session from the working copy, for example `Rscript --vanilla "MainAnalysis/Generate_afforestation_reforestation_rasterfile.R"` after adapting its paths. Generate both time points and modes before area statistics. Check the AOH worker error CSVs and confirm the expected species outputs and raster alignment before aggregation.

## Reproduction instructions

See the [workflow and figure mapping](docs/workflow-audit.md) for execution order, input/output contracts, and missing staging or preprocessing. A complete global rerun requires several intermediate products or their original producers as well as the third-party source data. The [submission status](docs/submission-checklist.md) records the scope of verification and outstanding items.

## License

The source code is released under the [MIT License](LICENSE). Third-party data remain subject to their providers’ terms. [Public repository](https://github.com/michunrong124/Global-Forestation-impact-on-Biodivesity)
