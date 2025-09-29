library(terra)

# Step 1: Load historical land-use rasters
lu1900 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover1900.tif")
lu1920 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover1920.tif")
lu1940 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover1940.tif")
lu1960 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover1960_1.tif")
lu1980 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover1980_1.tif")
lu2000 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover2000_1.tif")
lu2020 <- rast("/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Landcover2020_1.tif")


# Combine rasters into a list
lu_list <- list("1900"=lu1900, "1920"=lu1920, "1940"=lu1940, "1960"=lu1960, "1980"=lu1980, "2000"=lu2000, "2020"=lu2020)


# Step 2: Initialize rasters to store results
last_non_forest_year <- rast(lu2020) # To store the last year of non-forest
last_non_forest_type <- rast(lu2020) # To store the land-use type in that year
last_non_forest_year[] <- NA  # Initialize with NA
last_non_forest_type[] <- NA  # Initialize with NA

# Step 3: Iterate through historical rasters (from latest to earliest)
for (year in rev(names(lu_list))) { # Start from the most recent year
  current_year <- as.numeric(year)
  current_raster <- lu_list[[year]]
  
  # Identify non-forest pixels (not code 44) in the current year
  is_non_forest <- (current_raster != 44)
  
  # Update for pixels that are currently non-forest and not yet assigned
  to_update <- is.na(last_non_forest_year) & is_non_forest & (lu2020 == 44)
  last_non_forest_year[to_update] <- current_year
  last_non_forest_type[to_update] <- current_raster[to_update]
}


# Step 4: Save results
writeRaster(last_non_forest_year, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_year.tif", overwrite=TRUE)
writeRaster(last_non_forest_type, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type.tif", overwrite=TRUE)

####All forestation area
last_non_forest_type[last_non_forest_type == 11 | last_non_forest_type == 128| last_non_forest_type == 99] <- NA

writeRaster(last_non_forest_type, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type2.tif", overwrite=TRUE)
a<-freq(last_non_forest_type)
a


######Find out forestation type
origin0<-lu1900
origin0<-mask(origin0,last_non_forest_type)
b<-freq(origin0)
origin1<-origin0
origin1[origin1==11]<-NA
b<-freq(origin1)

last_non_forest_type3<-mask(last_non_forest_type,origin1)
writeRaster(last_non_forest_type3, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type3.tif", overwrite=TRUE)


# Step 2: Create rasters for Afforestation, Reforestation, and Uncategorized types
Aff_Part1 <- origin1
Aff_Part1[!(origin1 == 55 | origin1 == 66 | origin1 == 77)] <- NA # Keep 55, 66, 77, set others to NA
Ref_Part1 <- origin1
Ref_Part1[origin1 != 44] <- NA # Keep only 44, set others to NA
Uncategory <- origin1
Uncategorized[!(origin1 == 22 | origin1 == 33)] <- NA # Keep 22, 33, set others to NA

writeRaster(origin1,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/3forestation_type1.tif")

plot(Aff_Part1)
plot(Ref_Part1)
plot(Uncategorized)

pnv<-rast("/Volumes/WD_BLACK/PotentialOriginalVegetation/dataverse_files-2/pnv_reclassify_Eckert.tif")

pnv <- resample(pnv, Uncategorized, method = "near")
pnv2<-mask(pnv,Uncategorized)
t<-freq(pnv2)

pnv3<-pnv2
pnv3[pnv3 == 128] <- 44
t<-freq(pnv3)
t

Aff_Part2<-pnv3
Aff_Part2[!(Aff_Part2 == 55 | Aff_Part2 == 66 )] <- NA # Keep 55, 66, set others to NA

Ref_Part2<-pnv3
Ref_Part2[!(Ref_Part2 == 44 )] <- NA # Keep 44 set others to NA

Aff_All<-mosaic(Aff_Part1,Aff_Part2)
Ref_All<-mosaic(Ref_Part1,Ref_Part2)
b<-freq(Ref_All)
b

c<-freq(Ref_Part1)
d<-freq(Ref_Part2)
c
d
writeRaster(Aff_All,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Afforestation_clip0.tif")
writeRaster(Ref_All,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Reforestation_clip0.tif")

TwoForestation<-mosaic(Aff_All,Ref_All)
writeRaster(TwoForestation,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/2forestation_type1.tif")

last_non_forest_type4<-mask(last_non_forest_type3,TwoForestation)
writeRaster(last_non_forest_type4, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type4.tif", overwrite=TRUE)

last_non_forest_type4_Aff<-mask(last_non_forest_type3,Aff_All)
writeRaster(last_non_forest_type4_Aff, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type4_Afforestation.tif", overwrite=TRUE)

last_non_forest_type4_Ref<-mask(last_non_forest_type3,Ref_All)
writeRaster(last_non_forest_type4_Ref, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type4_Reforestation.tif", overwrite=TRUE)

#####Natural Forest & Plantation Forest& Agroforestry
forest_type<-rast('/Volumes/WD_BLACK/Natural&Planting forest/10478678/Natu_Plant2020_Eckert2.tif')
forest_type[forest_type==0|forest_type==7]<-NA
forest_type2 <- resample(forest_type, Aff_All, method = "near")

Aff_All2<-mask(Aff_All,forest_type2)
Ref_All2<-mask(Ref_All,forest_type2)

a<-freq(Aff_All2)
a
b<-freq(Aff_All)
b

a<-freq(Ref_All2)
a
b<-freq(Ref_All)
b

writeRaster(Aff_All2,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Afforestation_clip_Final.tif",overwrite=TRUE)
writeRaster(Ref_All2,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/Reforestation_clip_Final.tif",overwrite=TRUE)

TwoForestation2<-mosaic(Aff_All2,Ref_All2)
writeRaster(TwoForestation2,filename = "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/2forestation_type_final.tif",overwrite=TRUE)


last_non_forest_type5<-mask(last_non_forest_type4,TwoForestation2)
writeRaster(last_non_forest_type5, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type5.tif", overwrite=TRUE)

last_non_forest_type5_Aff<-mask(last_non_forest_type4,Aff_All2)
last_non_forest_type5_Ref<-mask(last_non_forest_type4,Ref_All2)

writeRaster(last_non_forest_type5_Aff, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type5_Afforestation.tif", overwrite=TRUE)
writeRaster(last_non_forest_type5_Ref, "/Volumes/WD_BLACK/GlobalLanduseData/LanduseChange_1899-2019/V2/last_non_forest_type5_Reforestation.tif", overwrite=TRUE)


LandcoverMid_affClip<-last_non_forest_type5_Aff
LandcoverMid_refClip<-last_non_forest_type5_Ref

writeRaster(LandcoverMid_affClip, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/LandcoverMid_affClip.tif", overwrite=TRUE)
writeRaster(LandcoverMid_refClip, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/LandcoverMid_refClip.tif", overwrite=TRUE)


Landcover2020_affClip<-mask(forest_type2,last_non_forest_type5_Aff)
Landcover2020_refClip<-mask(forest_type2,last_non_forest_type5_Ref)


Landcover2020_affClip2<-Landcover2020_affClip
Landcover2020_affClip2[Landcover2020_affClip2 == 1|Landcover2020_affClip2 == 2] <- 44
Landcover2020_affClip2[Landcover2020_affClip2 == 3|Landcover2020_affClip2 == 4|Landcover2020_affClip2 == 5] <- 45
Landcover2020_affClip2[Landcover2020_affClip2 == 6] <- 46

Landcover2020_refClip2<-Landcover2020_refClip
Landcover2020_refClip2[Landcover2020_refClip2 == 1|Landcover2020_refClip2 == 2] <- 44
Landcover2020_refClip2[Landcover2020_refClip2 == 3|Landcover2020_refClip2 == 4|Landcover2020_refClip2 == 5] <- 45
Landcover2020_refClip2[Landcover2020_refClip2 == 6] <- 46


writeRaster(Landcover2020_affClip2, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/Landcover2020_affClip.tif", overwrite=TRUE)
writeRaster(Landcover2020_refClip2, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/Landcover2020_refClip.tif", overwrite=TRUE)



#####Original landcover clip


Landcover1900_affClip<-mask(Aff_All2,last_non_forest_type5_Aff)
Landcover1900_refClip<-mask(Ref_All2,last_non_forest_type5_Ref)



writeRaster(Landcover1900_affClip, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/Landcover1900_affClip.tif", overwrite=TRUE)
writeRaster(Landcover1900_refClip, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/Landcover1900_refClip.tif", overwrite=TRUE)



#####1900 Winkler clip


Landcover1900_affClip<-mask(lu1900,last_non_forest_type5_Aff)
Landcover1900_refClip<-mask(lu1900,last_non_forest_type5_Ref)

a<-freq(Landcover1900_affClip)
sum(subset(a,value==22|value==33)$count)/sum(a$count)*100
###17.23912
b<-freq(Landcover1900_refClip)
 sum(subset(b,value==22|value==33)$count)/sum(b$count)*100
###57.32314

b<-freq(Landcover1900_refClip)
sum(subset(b,value==22|value==33)$count)/sum(b$count)*100

  
writeRaster(Landcover1900_affClip, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/Landcover1900_affClip_Old3Cat.tif", overwrite=TRUE)
writeRaster(Landcover1900_refClip, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/Landcover1900_refClip_Old3Cat.tif", overwrite=TRUE)


a<-freq(Landcover2020_affClip2)
a

b<-freq(Landcover2020_refClip2)
b
