This repository holds code for the following manuscript:


Chunrong Mi*, Alex Wiebe, Tong Mu, Dan Liang, David S. Wilcove, _2026_ Global impacts of afforestation and reforestation on biodiversity

michunrong123@qq.com/cm2603@princeton.edu, Robertson Hall, Princeton University, Princeton, NJ

In this analysis, we quantified the afforestatoin and reforestaton area (1899-2020) and their impact on biodiversity.

We used several datasets in this analysis. The long-term land-cover maps are from Winkler et al 2021, natural vegetation maps are from Hengl et al. 2018, and forest type maps are from Xu et al. 2024. Bird distribution maps are from BirdLife International (http://datazone.birdlife.org/); amphibian, reptile, and mammal distribution maps are from the IUCN (https://www.iucnredlist.org/resources/spatial-data-download). Species assessment data (including habitat and elevation preferences) are sourced from IUCN (https://www.iucnredlist.org/). Continent shape file data are from https://hub.arcgis.com/datasets/esri::world-continents/about. Elevation data are available at https://www.worldclim.org/data/worldclim21.html.

Winkler, K., Fuchs, R., Rounsevell, M. & Herold, M. Global land use changes are four times greater than previously estimated. Nat. Commun. 12, 2501 (2021).
Hengl, T. et al. Global mapping of potential natural vegetation: an assessment of machine learning algorithms for estimating land potential. PeerJ 6, e5457 (2018).
Xu, H. et al. Changes in the Fine Composition of Global Forests from 2001 to 2020. J. Remote Sens. 4, (2024).

Overview
There are three main components to this analysis. All scripts noted below can be found in the /scripts/ folder of this repository.

1. Primary analysis of range loss to species
Second, the core of our analysis is the calculation of range loss to species attributable to individual countries or across all countries. These scripts also include some code for secondary analysis (e.g., some summary statistics).

driver.R
driver_domestic.R

aohcalculations.R
functions.R

2. Supplementary analyses
We perform additional analyses on these outputs, for example to calculate summary statistics.

additional_analysis.R
criticallyendangered.R
OBL_sensitivity_analysis_driver.R
OBL_sensitivity_aohcalculations.R

3. Figure creation
fig2.R
fig3.R
fig3_part2.R
fig4.R
supp_fig1.R
supp_fig2_totallosses.R
supp_fig4.R
supp_fig_logratio.R

Computational requirements
We ran all code using R version 4.3 on Princeton University’s High Performance Computing cluster. The code may be run on a computer with sufficient RAM to accommodate the in-memory operations. Some scripts have considerable computational requirements (>100 gb RAM, >100 hrs total continuous runtime across all species). An example slurm script (‘OBL_ex_slurm’) is housed in this repository.
