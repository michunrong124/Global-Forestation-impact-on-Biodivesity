##################Draw figures for Afforestation, Forestation and Reforestaoin in 10km*10km from 1km*1km
###################@Mcr 20241225

rm(list = ls())
library(terra)
library(terra)
library(magick)
library(rworldmap)
library(rworldxtra)

Aff<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/Landcover2020_affClip.tif")
#Aff[Aff[]==128]<-NA

Aff[!is.na(Aff[])] <- 1
# ?????µ?դ???ֱ???Ϊ 10km x 10km
new_res <- 10

# ʹ?? aggregate ??????դ???ֱ??ʵ???Ϊ 10km x 10km????????ÿ???µ?Ԫ????ֵΪ 1 ?ĵ?Ԫ????��
Aff_10km <- aggregate(Aff, fact=new_res, fun=sum, na.rm=TRUE)
plot(Aff_10km)
#writeRaster(Aff_10km,filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Aff&Ref_Distribution/AfforestationOnly_1225.tif",overwrite=TRUE)


#####Reforestation

#rm(list = ls())
library(terra)
Ref<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/Landcover2020_refClip.tif")

Ref[!is.na(Ref[])] <- 1
# ?????µ?դ???ֱ???Ϊ 10km x 10km
new_res <- 10

Ref_10km <- aggregate(Ref, fact=new_res, fun=sum, na.rm=TRUE)
#plot(Ref_10km)
writeRaster(Ref_10km,filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Aff&Ref_Distribution/ReforestationOnly_1225.tif",overwrite=TRUE)



library(terra)

# checck：如果至少一个森林类型非零，其他森林类型的NA替换为0，但保留非零原始值
Aff_10km[is.na(Aff_10km) & (!is.na(Ref_10km))] <- 0
Ref_10km[is.na(Ref_10km) & (!is.na(Aff_10km))] <- 0
forest_stack <- c(Aff_10km, Ref_10km)
# 假设每个栅格的数值是森林类型的百分比 (0-100)，需要转换为0-1范围

Aff_norm <- Aff_10km / 100
Ref_norm <- Ref_10km / 100



forest_stack <- c(Aff_norm, Ref_norm)
dominant_forest <- which.max(forest_stack)

####important main figure
# define color：Afforestation - Red, Forest - Green, Reference - Blue
colors <- c("#85E5A7",  "#176533")

# plot
plot(dominant_forest, col = colors, main = "Dominant Forest Type")
#writeRaster(dominant_forest, filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Aff&Ref_Distribution/Dominant_ForestionType1225.tif",overwrite = TRUE)

#####

# calculate the percentage
total_percent <- Aff_norm + Ref_norm

# 避免除以零的情况
total_percent[total_percent == 0] <- NA

# 计算每种森林类型相对百分比
Aff_percent <- (Aff_norm / total_percent) * 100
Ref_percent <- (Ref_norm / total_percent) * 100

percent_stack <- c(Aff_percent,  Ref_percent)

# 命名每个波段
names(percent_stack) <- c("Afforestation",  "Reforestation")

plotRGB(c(Aff_percent, Ref_percent), r = 3, g = 2, b = 1, stretch="hist", smooth=TRUE,maxcell=1000000, main = "Forest Composition Map")

writeRaster(Aff_percent, filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Aff&Ref_Distribution/Afforestation_DistributionPercentage.tif",overwrite = TRUE)
writeRaster(Ref_percent, filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Aff&Ref_Distribution/Reforestation_DistributionPercentage.tif",overwrite = TRUE)
