rm(list = ls())
library(terra)

# ???ض??????????? Current
polygon<-vect("/Users/michunrong/Documents/Google/Paper/Global30Conservation/Polygon/fishnet10km_2.shp")

Amphibian<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/AmphibianSquare_Afforestation_TD.csv")
Amphibian$AmpChange<-Amphibian$SR2020-Amphibian$SRMid


####Reptile
Reptile<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/ReptileSquare_Afforestation_TD.csv")
Reptile$RepChange<-Reptile$SR2020-Reptile$SRMid



####Bird
Bird<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/BirdSquare_Afforestation_TD.csv")
Bird$BirdChange<-Bird$SR2020-Bird$SRMid

#######Mammal
Mammal<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/MammalSquare_Afforestation_TD.csv")
Mammal$MamChange<-Mammal$SR2020-Mammal$SRMid

All<-cbind(Amphibian,Reptile[,-1],Bird[,-1],Mammal[,-1])
All$Change<-All$AmpChange+All$RepChange+All$BirdChange+All$MamChange
# 将第2,4,8,9,11,13,15,16列的列索引存储在一个向量中
cols_to_check <- c(2,4,5,7,8,10,11,13)

# 使用 apply 函数遍历这些列，如果所有列的值都是0，则将 All$Change 设置为 NA
All$Change <- ifelse(rowSums(All[, cols_to_check] == 0) == length(cols_to_check), NA, All$Change)

polygon_grid <- merge(polygon,All,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/All_Afforestation_SRchange.tif", overwrite = TRUE)




#####Reforestation
rm(list = ls())
library(terra)

# ???ض??????????? Current
polygon<-vect("/Users/michunrong/Documents/Google/Paper/Global30Conservation/Polygon/fishnet10km_2.shp")

Amphibian<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/AmphibianSquare_Reforestation_TD.csv")
Amphibian$AmpChange<-Amphibian$SR2020-Amphibian$SRMid


####Reptile
Reptile<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/ReptileSquare_Reforestation_TD.csv")
Reptile$RepChange<-Reptile$SR2020-Reptile$SRMid



####Bird
Bird<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/BirdSquare_Reforestation_TD.csv")
Bird$BirdChange<-Bird$SR2020-Bird$SRMid

#######Mammal
Mammal<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/MammalSquare_Reforestation_TD.csv")
Mammal$MamChange<-Mammal$SR2020-Mammal$SRMid

All<-cbind(Amphibian,Reptile[,-1],Bird[,-1],Mammal[,-1])
All$Change<-All$AmpChange+All$RepChange+All$BirdChange+All$MamChange
# 将第2,4,8,9,11,13,15,16列的列索引存储在一个向量中
cols_to_check <- c(2,4,5,7,8,10,11,13)

# 使用 apply 函数遍历这些列，如果所有列的值都是0，则将 All$Change 设置为 NA
All$Change <- ifelse(rowSums(All[, cols_to_check] == 0) == length(cols_to_check), NA, All$Change)

polygon_grid <- merge(polygon,All,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/All_Reforestation_SRchange.tif", overwrite = TRUE)
