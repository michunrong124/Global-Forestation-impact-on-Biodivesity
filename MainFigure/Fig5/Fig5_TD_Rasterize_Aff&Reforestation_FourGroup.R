rm(list = ls())
library(terra)

# ???ض??????????? Current
polygon<-vect("/Users/michunrong/Documents/Google/Paper/Global30Conservation/Polygon/fishnet10km_2.shp")
Amphibian<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/AmphibianSquare_Afforestation_TD.csv")
Amphibian$Change<-Amphibian$SR2020-Amphibian$SRMid

Amphibian$Change <- ifelse(Amphibian$SRMid == 0 & Amphibian$SR2020 == 0, NA, Amphibian$Change)

polygon_grid <- merge(polygon,Amphibian,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Amphibian_Afforestation_SRchange.tif", overwrite = TRUE)

####Reptile
Reptile<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/ReptileSquare_Afforestation_TD.csv")
Reptile$Change<-Reptile$SR2020-Reptile$SRMid
Reptile$Change<-Reptile$SR2020-Reptile$SRMid

Reptile$Change <- ifelse(Reptile$SRMid == 0 & Reptile$SR2020 == 0, NA, Reptile$Change)

polygon_grid <- merge(polygon,Reptile,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Reptile_Afforestation_SRchange.tif", overwrite = TRUE)


####Bird
Bird<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/BirdSquare_Afforestation_TD.csv")
Bird$Change<-Bird$SR2020-Bird$SRMid

Bird$Change <- ifelse(Bird$SRMid == 0 & Bird$SR2020 == 0, NA, Bird$Change)

polygon_grid <- merge(polygon,Bird,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Bird_Afforestation_SRchange.tif", overwrite = TRUE)


#######Mammal
Mammal<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/MammalSquare_Afforestation_TD.csv")
Mammal$Change<-Mammal$SR2020-Mammal$SRMid

Mammal$Change <- ifelse(Mammal$SRMid == 0 & Mammal$SR2020 == 0, NA, Mammal$Change)

polygon_grid <- merge(polygon,Mammal,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Mammal_Afforestation_SRchange.tif", overwrite = TRUE)




####Reforestation
rm(list = ls())
library(terra)

# ???ض??????????? Current
polygon<-vect("/Users/michunrong/Documents/Google/Paper/Global30Conservation/Polygon/fishnet10km_2.shp")
Amphibian<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/AmphibianSquare_Reforestation_TD.csv")
Amphibian$Change<-Amphibian$SR2020-Amphibian$SRMid

Amphibian$Change <- ifelse(Amphibian$SRMid == 0 & Amphibian$SR2020 == 0, NA, Amphibian$Change)

polygon_grid <- merge(polygon,Amphibian,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Amphibian_Reforestation_SRchange.tif", overwrite = TRUE)

####Reptile
Reptile<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/ReptileSquare_Reforestation_TD.csv")
Reptile$Change<-Reptile$SR2020-Reptile$SRMid
Reptile$Change<-Reptile$SR2020-Reptile$SRMid

Reptile$Change <- ifelse(Reptile$SRMid == 0 & Reptile$SR2020 == 0, NA, Reptile$Change)

polygon_grid <- merge(polygon,Reptile,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Reptile_Reforestation_SRchange.tif", overwrite = TRUE)


####Bird
Bird<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/BirdSquare_Reforestation_TD.csv")
Bird$Change<-Bird$SR2020-Bird$SRMid

Bird$Change <- ifelse(Bird$SRMid == 0 & Bird$SR2020 == 0, NA, Bird$Change)

polygon_grid <- merge(polygon,Bird,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Bird_Reforestation_SRchange.tif", overwrite = TRUE)


#######Mammal
Mammal<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/MammalSquare_Reforestation_TD.csv")
Mammal$Change<-Mammal$SR2020-Mammal$SRMid

Mammal$Change <- ifelse(Mammal$SRMid == 0 & Mammal$SR2020 == 0, NA, Mammal$Change)

polygon_grid <- merge(polygon,Mammal,by.x="Id",by.y="id")

raster_template <- rast(ext(polygon_grid), resolution = 10000, crs = crs(polygon_grid))
rasterized_grid <- rasterize(polygon_grid, raster_template, field = "Change")
plot(rasterized_grid)
writeRaster(rasterized_grid, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Mammal_Reforestation_SRchange.tif", overwrite = TRUE)
