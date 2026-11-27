# Nature Research Code and Software Submission Checklist status

Scope follows the [Nature Research code publication guidelines](https://media.nature.com/full/nature-cms/documents/GuidelinesCodePublication.pdf).

| Item | Status | Evidence or open requirement |
| --- | --- | --- |
| Source code and README | Available for review | 50 original R scripts and the [README](../README.md). Coverage checked against the October 27, 2026 Nature Sustainability submission. See the workflow audit for gaps. |
| System requirements and tested versions | Verified for demo only | README and environment below. The submission specification is R 4.3.2 for McNemar tests. The older R 4.5.0 HPC report and historical cluster OS, packages, and hardware figures remain unverified. |
| Install instructions and timing | Verified for R packages | [Installer](../install_packages.R), commands below, and observed elapsed times. Full HPC and figure-renderer setup remain untested. |
| Example dataset, expected output, and runtime | Verified for representative AOH workflow | [Synthetic input generator and runner](../demo/run_demo.R) and [expected results](../demo/expected_aoh.csv). |
| Instructions for use on other data | Documented | README and [script-level workflow audit](workflow-audit.md). The global workflow depends on preprocessing and intermediates listed in the audit. |
| Full quantitative reproduction (optional) | Open | Requires data access and provenance, missing producers or intermediate files, and a global rerun. |
| License and public repository | Available | [MIT License](../LICENSE) and [public repository](https://github.com/michunrong124/Global-Forestation-impact-on-Biodivesity). Confirm the derived GeoTIFF redistribution terms separately. |
| Manuscript location of functionality | Located in inspected draft | The October Nature Sustainability Methods draft describes classification, AOH, change metrics, and statistical analysis. Its Code availability section links to the repository. Align the Code availability statement with the audited workflow before release. |

## Verification on the test Mac

- Apple M3 Max, 64 GiB RAM, macOS 26.6.2 (25G83), R 4.5.1 (arm64).
- Demo used raster 3.6-32, terra 1.9-60, sp 2.2-1, and Rcpp 1.1.1-1.1. The terra binary reported GDAL 3.8.5, GEOS 3.13.0, PROJ 9.5.1, and TBB disabled.
- `/usr/bin/time -p Rscript --vanilla install_packages.R demo` completed in 4.24 seconds from a clean checkout, and in 4.40 seconds in an earlier trial, each using an empty target library. The shared R library already supplied sp and Rcpp.
- `/usr/bin/time -p Rscript --vanilla install_packages.R analysis` completed in 8.64 seconds in a separate empty target library, with some transitive dependencies already in the shared R library. Direct packages installed: data.table 1.18.6.1, dplyr 1.2.1, ggplot2 4.0.4, gridExtra 2.3.1, htmlwidgets 1.6.4, magick 2.9.1, openxlsx 4.2.9, plotly 4.12.1, raster 3.6-32, reshape2 1.4.5, rworldmap 1.3-8, rworldxtra 1.01, sf 1.1-3, snowfall 1.84-6.3, stringr 1.6.0, terra 1.9-60, tidyr 1.3.2, tidyverse 2.0.0, and webshot 0.5.5.
- All 19 direct analysis package namespaces loaded from the test environment. PhantomJS was detected and Pandoc was unavailable there. Fig. 2 export remains unverified.
- `/usr/bin/time -l Rscript --vanilla demo/run_demo.R` took 9.44 seconds in the clean checkout, with maximum resident set size about 398 MiB. The demo assertions and an independent comparison of the two CSVs matched all 18 expected rows. R warned that sp and terra were built under R 4.5.2. The demo completed on R 4.5.1 despite those warnings. Global analysis and figure reproduction remain unverified.
