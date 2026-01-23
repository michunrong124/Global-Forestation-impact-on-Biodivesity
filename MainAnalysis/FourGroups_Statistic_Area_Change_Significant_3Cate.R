####################Calculate the significance of range change 
#########@Mcr 20240926

rm(list = ls())
library(openxlsx)
#library(exact2x2)
library(parallel)
library(dplyr)
library(tidyverse)
library(data.table)

groups<-c("Amphibian","Reptile","Bird","Mammal")
forest_types<-c("Natural","Plantation","Agroforestry")
#################Afforestation 
####Natural

myfunction<-function(group,forest_type) {
df<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/",group,"_Afforestation_Area_",forest_type,".csv"))
df <- df[!(df$YMidAll == 0 & df$Y2020All == 0), ]
IUCN_Cate<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_",group,"_search_Reasult/assessments.csv"))
IUCN_Cate<-IUCN_Cate[,c("scientificName","redlistCategory")]
df<-merge(df,IUCN_Cate,by.x="Species",by.y="scientificName")
df<-subset(df,redlistCategory != "Extinct" & redlistCategory != "Extinct in the Wild")

df$AllArea<-1645948
row.names(df)<-df[,1]

MySig<-function(i){
  
  # ???? a, b, c, d
  a <- df$Overlap[i] #####current sutiable, future suitable
  b <- df$YMid[i]                   #####current suitable, future unsuitable          
  c <- df$Y2020[i]                  #####current unsuitable, future suitable
  d <- df$AllArea[i]-a-b-c          #####current unsuitable, future unsuitable
  change=c-b
  matrix_data <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
  
  #print(matrix_data)
  # Perform McNemar's test
  test_result <- mcnemar.test(matrix_data, correct = TRUE)
  #test_result <- exact2x2(matrix_data)
  result<-data.frame(cbind(row.names(df)[i],change, test_result$p.value,df$redlistCategory[i]))
  names(result)<-c("species","change","p-value","redlistCategory")
  result <- result %>%
    mutate_all(~ ifelse(is.nan(.), 1, .))
  
  return(result)
}

result2<-mclapply(1:nrow(df), MySig,mc.cores=4)
#result2<-mclapply(1:10, MySig,mc.cores=5)

result3=do.call(rbind,result2)
result3[,"p-value"]<-as.numeric(result3[,"p-value"])
result3[,"p-value"][is.nan(result3[,"p-value"])] <- 1

write.csv(result3,file=paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/Statistic_Result/",group,"_Afforestation_RangeChangeTrend_Significant_",forest_type,".csv"),row.names = F)
}


# 并行处理，i 和 j 独立控制
mclapply(1:length(groups), function(i) {
  for (j in 1:length(forest_types)) {
    myfunction(groups[i], forest_types[j])  # 分别调用 groups[i] 和 afforestation_types[j]
  }
}, mc.cores = 4)



######Reforestation
myfunction<-function(group,forest_type) {
  df<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/",group,"_Reforestation_Area_",forest_type,".csv"))
  df <- df[!(df$YMidAll == 0 & df$Y2020All == 0), ]
  IUCN_Cate<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_",group,"_search_Reasult/assessments.csv"))
  IUCN_Cate<-IUCN_Cate[,c("scientificName","redlistCategory")]
  df<-merge(df,IUCN_Cate,by.x="Species",by.y="scientificName")
  df<-subset(df,redlistCategory != "Extinct" & redlistCategory != "Extinct in the Wild")
  
  df$AllArea<-2067650
  row.names(df)<-df[,1]
  
  MySig<-function(i){
    
    # ???? a, b, c, d
    a <- df$Overlap[i] #####current sutiable, future suitable
    b <- df$YMid[i]                   #####current suitable, future unsuitable          
    c <- df$Y2020[i]                  #####current unsuitable, future suitable
    d <- df$AllArea[i]-a-b-c          #####current unsuitable, future unsuitable
    change=c-b
    matrix_data <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
    
    #print(matrix_data)
    # Perform McNemar's test
    test_result <- mcnemar.test(matrix_data, correct = TRUE)
    #test_result <- exact2x2(matrix_data)
    result<-data.frame(cbind(row.names(df)[i],change, test_result$p.value,df$redlistCategory[i]))
    names(result)<-c("species","change","p-value","redlistCategory")
    result <- result %>%
      mutate_all(~ ifelse(is.nan(.), 1, .))
    
    return(result)
  }
  
  result2<-mclapply(1:nrow(df), MySig,mc.cores=4)
  #result2<-mclapply(1:10, MySig,mc.cores=5)
  
  result3=do.call(rbind,result2)
  result3[,"p-value"]<-as.numeric(result3[,"p-value"])
  result3[,"p-value"][is.nan(result3[,"p-value"])] <- 1
  
  write.csv(result3,file=paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/Statistic_Result/",group,"_Reforestation_RangeChangeTrend_Significant_",forest_type,".csv"),row.names = F)
}


mclapply(1:length(groups), function(i) {
  for (j in 1:length(forest_types)) {
    myfunction(groups[i], forest_types[j])  
  }
}, mc.cores = 4)


