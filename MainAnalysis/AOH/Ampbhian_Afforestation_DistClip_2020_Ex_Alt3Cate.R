rm(list = ls())
library(raster)
library(sf)
library(dplyr)
library(terra)
library(parallel)
library(snowfall)


#plot(land_use, main="Land Use")


input_dir <- "/scratch/gpfs/cm2603/SpeciesDistribution/AOH/Amphibian/"
out_dirN<-"/scratch/gpfs/cm2603/Afforestation/Afforestation_Distribution_Map3/2020/Amphibian/Natural/"
out_dirP<-"/scratch/gpfs/cm2603/Afforestation/Afforestation_Distribution_Map3/2020/Amphibian/Plantation/"
out_dirA<-"/scratch/gpfs/cm2603/Afforestation/Afforestation_Distribution_Map3/2020/Amphibian/Agroforestry/"



altitude<-raster("/scratch/gpfs/cm2603/Afforestation/altitudeEckert.tif")
land_use <- raster("/scratch/gpfs/cm2603/Afforestation/Afforestation_Clip/Landcover2020_affClip.tif")

land_useN <- land_use; land_useP <- land_use; land_useA <- land_use
land_useN[land_useN != 44] <- NA
land_useP[land_useP != 45] <- NA
land_useA[land_useA != 46] <- NA


    

Habitat<-read.csv("/scratch/gpfs/cm2603/Afforestation/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine6_1.csv")

###########Clip altitude 
############load Species distributin with polygon

site<-1:nrow(Habitat)

extract_actual_distribution <- function(potential_distribution, land_use, allowed_land_use_codes) {
  #potential_distribution <- shapefile(paste0(input_dir,species_name,".shp",sep=""))
  cropped_land_use <- crop(land_use, potential_distribution)
  cropped_land_use <-mask(cropped_land_use,potential_distribution)
  actual_distribution <- calc(cropped_land_use, fun=function(x) {ifelse(x %in% allowed_land_use_codes, 1, NA)})
  return(actual_distribution)
}


MyBiomodSFN<-function(i){
  
  TemSpecies<-Habitat[i,"scientificName"]
  FilterAlt3<-raster(paste0(input_dir,TemSpecies,".tif",sep=""))

  #########Land cover 

  FilterLand1<-crop(land_useN,FilterAlt3)
  FilterAlt3_resampled <- resample(FilterAlt3, FilterLand1, method = "ngb")
  FilterLand1<-mask(FilterLand1,FilterAlt3_resampled)
  
  land_use_codes <- Habitat$code[[i]]
  land_use_codes <- as.integer(strsplit(land_use_codes, ", ")[[1]])
  actual_distribution <- extract_actual_distribution(FilterLand1, land_useN, land_use_codes)
  writeRaster(actual_distribution, paste0(out_dirN, TemSpecies, ".tif"), overwrite=TRUE)
  
  }
  
  
MyBiomodSFN1<-function(i){      ####scan error
  blank<-vector()
  test<-try(MyBiomodSFN(i))
  if("try-error" %in% class(test)) write.csv(blank,paste(out_dirN,i,"_",Habitat$scientificName[i], ' is error',".csv",sep=""))
  return(test)
}



MyBiomodSFP<-function(i){
  
  TemSpecies<-Habitat[i,"scientificName"]
  FilterAlt3<-raster(paste0(input_dir,TemSpecies,".tif",sep=""))
  
  #########Land cover 
  
  FilterLand1<-crop(land_useP,FilterAlt3)
  FilterAlt3_resampled <- resample(FilterAlt3, FilterLand1, method = "ngb")
  FilterLand1<-mask(FilterLand1,FilterAlt3_resampled)
  
  land_use_codes <- Habitat$code[[i]]
  land_use_codes <- as.integer(strsplit(land_use_codes, ", ")[[1]])
  actual_distribution <- extract_actual_distribution(FilterLand1, land_useP, land_use_codes)
  writeRaster(actual_distribution, paste0(out_dirP, TemSpecies, ".tif"), overwrite=TRUE)
  
}


MyBiomodSFP1<-function(i){      ####scan error
  blank<-vector()
  test<-try(MyBiomodSFP(i))
  if("try-error" %in% class(test)) write.csv(blank,paste(out_dirP,i,"_",Habitat$scientificName[i], ' is error',".csv",sep=""))
  return(test)
}



MyBiomodSFA<-function(i){
  
  TemSpecies<-Habitat[i,"scientificName"]
  FilterAlt3<-raster(paste0(input_dir,TemSpecies,".tif",sep=""))
  
  #########Land cover 
  
  FilterLand1<-crop(land_useA,FilterAlt3)
  FilterAlt3_resampled <- resample(FilterAlt3, FilterLand1, method = "ngb")
  FilterLand1<-mask(FilterLand1,FilterAlt3_resampled)
  
  land_use_codes <- Habitat$code[[i]]
  land_use_codes <- as.integer(strsplit(land_use_codes, ", ")[[1]])
  actual_distribution <- extract_actual_distribution(FilterLand1, land_useA, land_use_codes)
  writeRaster(actual_distribution, paste0(out_dirA, TemSpecies, ".tif"), overwrite=TRUE)
  
}


MyBiomodSFA1<-function(i){      ####scan error
  blank<-vector()
  test<-try(MyBiomodSFA(i))
  if("try-error" %in% class(test)) write.csv(blank,paste(out_dirA,i,"_",Habitat$scientificName[i], ' is error',".csv",sep=""))
  return(test)
}






###################################################
### code chunk number 11: SnowFold_3 (eval = FALSE)
###################################################
## 
## ## Init snowfall
sfInit(parallel=TRUE, cpus=10 )  ## we select 2 CPUs. If you have 8 CPUs, put 8. 

## Export packages
sfLibrary('raster', character.only=TRUE)
sfLibrary('stringr', character.only=TRUE)
sfLibrary('sf', character.only=TRUE)
sfLibrary('dplyr', character.only=TRUE)
sfLibrary('terra', character.only=TRUE)
sfLibrary('snowfall', character.only=TRUE)

## 
## ## Export variables
#sfExport('DataSpecies')
sfExportAll()

## 
## # you may also use sfExportAll() to export all your workspace variables
## 
## ## Do the run
#mySFModelsOut <- sfLapply(site, MyBiomodSF1)
mySFModelsOut <- sfLapply(site, MyBiomodSFN1)
mySFModelsOut <- sfLapply(site, MyBiomodSFP1)
mySFModelsOut <- sfLapply(site, MyBiomodSFA1)
## 
## ## stop snowfall
sfStop( nostop=FALSE )
#end_time=Sys.time()
#end_time-start_time
