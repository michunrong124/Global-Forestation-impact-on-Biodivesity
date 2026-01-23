rm(list = ls())

library(terra)


# Step 1: Load historical land-use rasters
lu1900 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover1900.tif")
last_non_forest_type<-rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type2.tif")
######Find out forestation type
origin0<-lu1900
origin0<-mask(origin0,last_non_forest_type)
origin1<-origin0
origin1[origin1==11]<-NA  ###remove Urban Area


Landcover2020_affClip2<-rast('/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/Landcover2020_affClip.tif')
Landcover2020_refClip2<-rast('/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/Landcover2020_refClip.tif')
Landcover2020<-mosaic(Landcover2020_affClip2,Landcover2020_refClip2)

Landcover2020_2<-mask(origin1,Landcover2020)
Landcover2020_3<-Landcover2020_2
Landcover2020_3[Landcover2020_3 == 22|Landcover2020_3 == 33] <- 1
Landcover2020_3[Landcover2020_3 == 44|Landcover2020_3 == 55|Landcover2020_3 == 66|Landcover2020_3 == 77] <- 2
writeRaster(Landcover2020_3, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/NaturalLandCover_ActiveHumanUse_AfterForestType.tif", overwrite=TRUE)

tt<-freq(Landcover2020_3)
print(tt[1,3]/sum(tt[,3])*100)  ####Active human landuse in forestation area





