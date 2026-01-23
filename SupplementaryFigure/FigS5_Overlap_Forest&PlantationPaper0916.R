rm(list = ls())
library(terra)
land2020<-rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover2020_1.tif")
land2020[land2020!=44]<-NA
#writeRaster(land2020,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Forest2020_1.tif",overwrite=TRUE)
Natu_Plant<-rast("/Volumes/WD_BLACK/Natural&Planting forest/10478678/Natu_Plant2020_Eckert.tif")

Natu_Plant_resampled <- resample(Natu_Plant,land2020,method="near")


#combined_raster <- overlay(land2020, Natu_Plant_resampled, fun=function(x, y) {
#  ifelse(is.na(x), y, ifelse(is.na(y), x, x + y))
#})

combined_raster <- ifel(!is.na(land2020), 
                        ifel(!is.na(Natu_Plant_resampled), land2020 + Natu_Plant_resampled, land2020),
                        Natu_Plant_resampled)

c<-freq(combined_raster)

combined_raster2<-combined_raster
combined_raster2[combined_raster2<45|combined_raster2>50]<-NA
combined_raster3<-combined_raster2-44

d<-freq(combined_raster3)

#writeRaster(combined_raster3,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Sixtype_Forest2020_Location.tif",overwrite=TRUE)


######Find out the region landcovrer identify as forest but new forest types not.
combined_raster_x<-combined_raster
combined_raster_x[combined_raster_x<8]<-NA
combined_raster_x[combined_raster_x>44&combined_raster_x<51]<-NA

combined_raster_x2<-combined_raster_x
combined_raster_x2[combined_raster_x2==44|combined_raster_x2==51]<-1

t<-freq(combined_raster_x2)

#writeRaster(combined_raster_x2,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Forestlandcover_NotIdentify2020_Location.tif",overwrite=TRUE)



combined_raster3<-rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Sixtype_Forest2020_Location.tif")
# ???·???
# 1, 2 -> 44; 47, 48 -> 45
combined_raster4<- classify(combined_raster3, rbind(c(1, 44), c(2, 44), c(3, 45), c(4, 45), c(5, 45),c(6, 46)))
t<-freq(combined_raster4)
# ????????

MatchForestArea<-sum(t[,3])

#writeRaster(combined_raster4,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Sixtype_Forest2020_Location_3Cate.tif",overwrite=TRUE)


land2020Area<-freq(land2020)

Natu_Plant_resampled[Natu_Plant_resampled==0]<-NA
Natu_Plant_resampled[Natu_Plant_resampled==7]<-NA
Natu_Plant_resampledArea<-freq(Natu_Plant_resampled)

MatchForestArea/land2020Area[,3]*100   ###79.8978%  Match Forest occupy landcover 2020 Whinkl
MatchForestArea/sum(Natu_Plant_resampledArea[,3])*100   ###73.66067%  Match Forest occupy different forest 2020 Xu et al.



####forest only in Land cover 2020
land2020_2<-land2020
land2020_2[land2020_2==44]<-99

vals_to_mask <- c(44,45,46)

ForOnlyInland2020 <- mask(land2020_2, combined_raster4 %in% vals_to_mask, maskvalue=TRUE)

ForOnlyInLand2020Area<-freq(ForOnlyInland2020)
(ForOnlyInLand2020Area/land2020Area*100)[,3]
####forest only in forest 2020
ForOnlyInFor2020<- mask(Natu_Plant_resampled, combined_raster4 %in% vals_to_mask, maskvalue=TRUE)
ForOnlyInFor2020<- classify(ForOnlyInFor2020, rbind(c(1, 44), c(2, 44), c(3, 45), c(4, 45), c(5, 45),c(6, 46)))

ForOnlyInFor2020Area<-sum(freq(ForOnlyInFor2020)[,3])

ForOnlyInFor2020Area/sum(Natu_Plant_resampledArea[,3])*100 


MisMatch <- cover(ForOnlyInland2020, ForOnlyInFor2020)
MisMatch2<- classify(MisMatch, rbind(c(44, 1), c(45, 1), c(46, 1), c(99, 99)))



writeRaster(MisMatch,filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/ForestMismatch/ForestMismatch.tif",overwrite=TRUE)
writeRaster(MisMatch2,filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/ForestMismatch/ForestMismatch2.tif",overwrite=TRUE)
writeRaster(combined_raster4,filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/ForestMismatch/ForestMatch.tif",overwrite=TRUE)

