This repository holds code for the following manuscript:


Chunrong Mi*, Alex Wiebe, Tong Mu, Dan Liang, David S. Wilcove, _2026_ Global impacts of afforestation and reforestation on biodiversity

michunrong123@qq.com/cm2603@princeton.edu, Robertson Hall, Princeton University, Princeton, NJ

In this analysis, we quantified global afforestatoin and reforestaton (1899-2020) distribution, size, and their impact on biodiversity.

We used several datasets in this analysis. The long-term land-cover maps are from Winkler et al 2021, Nature communications
natural vegetation maps are from Hengl et al. 2018, PeerJ; and forest type maps are from Xu et al. 2024. Journal of Remote Sensing.
Bird distribution maps are from BirdLife International (http://datazone.birdlife.org/); 
amphibian, reptile, and mammal distribution maps are from the IUCN (https://www.iucnredlist.org/resources/spatial-data-download). 
Species assessment data (including habitat and elevation preferences) are sourced from IUCN (https://www.iucnredlist.org/). 
Continent shape file data are from https://hub.arcgis.com/datasets/esri::world-continents/about. 
Elevation data are available at https://www.worldclim.org/data/worldclim21.html.

Winkler, K., Fuchs, R., Rounsevell, M. & Herold, M. Global land use changes are four times greater than previously estimated. Nat. Commun. 12, 2501 (2021).
Hengl, T. et al. Global mapping of potential natural vegetation: an assessment of machine learning algorithms for estimating land potential. PeerJ 6, e5457 (2018).
Xu, H. et al. Changes in the Fine Composition of Global Forests from 2001 to 2020. J. Remote Sens. 4, (2024).

Overview:
There are four main components to this paper. For main figure, main analysis and supplementary figure and supplementary table.

1. Main figure
Fig.1-Fig5
   
2. Main Analysis
i) Raster of afforestation and reforestaton area generate.
ii) Deal with habitat information from IUCN.
iii) AOH method for four terrestrial vertebrates (amphibians, reptiles, birds, mammals) under afforestation and reforestation for three forest types.
iv) Biodviersity trend statistic.
v) Some supplementary information.
   
3. Supplementary figure
Supplemetary FigS1,S3,S5-S8.

4. Supplementary Table
The crosswalk transform species habitat information from IUCN to landcover type.


Computational requirements
We ran all code using R version 4.5.0 on Princeton University’s High Performance Computing cluster. The code may be run on a computer with sufficient RAM to accommodate the in-memory operations. Some scripts have considerable computational requirements (>1000 gb RAM, >100 hrs total continuous runtime across all species). 
