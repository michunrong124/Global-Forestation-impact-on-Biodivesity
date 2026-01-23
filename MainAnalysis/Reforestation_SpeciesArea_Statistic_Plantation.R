############Reforestation Intersection of Mid & 2020 distribution#######
###########@Mcr 20240918, 5 Saylor Ct
rm(list = ls())
library(terra)
library(openxlsx)
library(snowfall)
library(parallel)
library(stringr)

Outdir<-'/scratch/gpfs/cm2603/Reforestation/Table/'

AmpList0<-list.files('/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Amphibian/Plantation/',pattern='\\.tif$', full=TRUE)
a<-basename(AmpList0)
nameList<-matrix(NA,length(AmpList0),1)
for(i in 1: length(AmpList0)){
  nameList[i]<-a[i]
  nameList[i]<-str_replace_all(strsplit(nameList[i],'.tif',fixed = TRUE)[[1]],' ',' ') 
}
nameList<-as.data.frame(nameList)
AmpList2<-unique(nameList)



RepList0<-list.files('/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Reptile/Plantation/',pattern='\\.tif$', full=TRUE)
a<-basename(RepList0)
nameList<-matrix(NA,length(RepList0),1)
for(i in 1: length(RepList0)){
  nameList[i]<-a[i]
  nameList[i]<-str_replace_all(strsplit(nameList[i],'.tif',fixed = TRUE)[[1]],' ',' ') 
}
nameList<-as.data.frame(nameList)
RepList2<-unique(nameList)


MamList0<-list.files('/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Mammal/Plantation/',pattern='\\.tif$', full=TRUE)
a<-basename(MamList0)
nameList<-matrix(NA,length(MamList0),1)
for(i in 1: length(MamList0)){
  nameList[i]<-a[i]
  nameList[i]<-str_replace_all(strsplit(nameList[i],'.tif',fixed = TRUE)[[1]],' ',' ') 
}
nameList<-as.data.frame(nameList)
MamList2<-unique(nameList)

BirdList0<-list.files('/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Bird/Plantation/',pattern='\\.tif$', full=TRUE)
a<-basename(BirdList0)
nameList<-matrix(NA,length(BirdList0),1)
for(i in 1: length(BirdList0)){
  nameList[i]<-a[i]
  nameList[i]<-str_replace_all(strsplit(nameList[i],'.tif',fixed = TRUE)[[1]],' ',' ') 
}
nameList<-as.data.frame(nameList)
BirdList2<-unique(nameList)


Afun <- function(i) {
  ref_raster <-rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Amphibian/Plantation/", AmpList2[i, 1], ".tif"))
  raster1 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Amphibian/Plantation/", AmpList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for Mid:", AmpList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  
  raster2 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/2020/Amphibian/Plantation/", AmpList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for 2020:", AmpList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  

  
  overlap <- raster1 == 1 & raster2 == 1
  exclusive1 <- raster1 == 1 & is.na(raster2)
  exclusive2 <- raster2 == 1 & is.na(raster1)
  
  overlap_count <- sum(values(overlap) == 1, na.rm = TRUE)
  exclusive1_count <- sum(values(exclusive1) == 1, na.rm = TRUE)
  exclusive2_count <- sum(values(exclusive2) == 1, na.rm = TRUE)
  

  result <- data.frame(
    Species = AmpList2[i, 1],
    YMid = exclusive1_count,
    Y2020 = exclusive2_count,
    Overlap = overlap_count,
    YMidAll = overlap_count + exclusive1_count,
    Y2020All = overlap_count + exclusive2_count
  )
  
  return(result)
}

Asite<-1:nrow(AmpList2)
AOut <- mclapply(Asite,Afun,mc.cores = 10)
Aresult<-do.call(rbind,AOut)
write.csv(Aresult,file =paste0(Outdir,'Amphibian_Reforestation_Area_Plantation.csv'),row.names = F)



Rfun <- function(i) {
  ref_raster <-rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Reptile/Plantation/", RepList2[i, 1], ".tif"))
  raster1 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Reptile/Plantation/", RepList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for Mid:", RepList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  
  raster2 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/2020/Reptile/Plantation/", RepList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for 2020:", RepList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  

  
  
  overlap <- raster1 == 1 & raster2 == 1
  exclusive1 <- raster1 == 1 & is.na(raster2)
  exclusive2 <- raster2 == 1 & is.na(raster1)
  
  overlap_count <- sum(values(overlap) == 1, na.rm = TRUE)
  exclusive1_count <- sum(values(exclusive1) == 1, na.rm = TRUE)
  exclusive2_count <- sum(values(exclusive2) == 1, na.rm = TRUE)
  
  result <- data.frame(
    Species = RepList2[i, 1],
    YMid = exclusive1_count,
    Y2020 = exclusive2_count,
    Overlap = overlap_count,
    YMidAll = overlap_count + exclusive1_count,
    Y2020All = overlap_count + exclusive2_count
  )
  
  return(result)
}

Rsite<-1:nrow(RepList2)
ROut <- mclapply(Rsite,Rfun,mc.cores = 10)
Rresult<-do.call(rbind,ROut)
write.csv(Rresult,file =paste0(Outdir,'Reptile_Reforestation_Area_Plantation.csv'),row.names = F)


Mfun <- function(i) {
  ref_raster <-rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Mammal/Plantation/", MamList2[i, 1], ".tif"))
  raster1 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Mammal/Plantation/", MamList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for Mid:", MamList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  
  raster2 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/2020/Mammal/Plantation/", MamList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for 2020:", MamList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  
  
  overlap <- raster1 == 1 & raster2 == 1
  exclusive1 <- raster1 == 1 & is.na(raster2)
  exclusive2 <- raster2 == 1 & is.na(raster1)
  
  overlap_count <- sum(values(overlap) == 1, na.rm = TRUE)
  exclusive1_count <- sum(values(exclusive1) == 1, na.rm = TRUE)
  exclusive2_count <- sum(values(exclusive2) == 1, na.rm = TRUE)
  
  
  result <- data.frame(
    Species = MamList2[i, 1],
    YMid = exclusive1_count,
    Y2020 = exclusive2_count,
    Overlap = overlap_count,
    YMidAll = overlap_count + exclusive1_count,
    Y2020All = overlap_count + exclusive2_count
  )
  
  return(result)
}


Msite<-1:nrow(MamList2)
MOut <- mclapply(Msite,Mfun,mc.cores = 10)
Mresult<-do.call(rbind,MOut)
write.csv(Mresult,file =paste0(Outdir,'Mammal_Reforestation_Area_Plantation.csv'),row.names = F)





Bfun <- function(i) {
  ref_raster <-rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Bird/Plantation/", BirdList2[i, 1], ".tif"))
  raster1 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/Mid/Bird/Plantation/", BirdList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for Mid:", BirdList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  
  raster2 <- tryCatch({
    rast(paste0("/scratch/gpfs/cm2603/Reforestation/Reforestation_Distribution_Map3/2020/Bird/Plantation/", BirdList2[i, 1], ".tif"))
  }, error = function(e) {
    message(paste("File not found for 2020:", BirdList2[i, 1], "- using zero raster"))
    return(rast(nrows = nrow(ref_raster), ncols = ncol(ref_raster), ext = ext(ref_raster), vals = 0))
  })
  
  
  overlap <- raster1 == 1 & raster2 == 1
  exclusive1 <- raster1 == 1 & is.na(raster2)
  exclusive2 <- raster2 == 1 & is.na(raster1)
  
  overlap_count <- sum(values(overlap) == 1, na.rm = TRUE)
  exclusive1_count <- sum(values(exclusive1) == 1, na.rm = TRUE)
  exclusive2_count <- sum(values(exclusive2) == 1, na.rm = TRUE)
  
  result <- data.frame(
    Species = BirdList2[i, 1],
    YMid = exclusive1_count,
    Y2020 = exclusive2_count,
    Overlap = overlap_count,
    YMidAll = overlap_count + exclusive1_count,
    Y2020All = overlap_count + exclusive2_count
  )
  
  return(result)
}





Bsite<-1:nrow(BirdList2)
BOut <- mclapply(Bsite,Bfun,mc.cores = 10)
Bresult<-do.call(rbind,BOut)
write.csv(Bresult,file =paste0(Outdir,'Bird_Reforestation_Area_Plantation.csv'),row.names = F)


