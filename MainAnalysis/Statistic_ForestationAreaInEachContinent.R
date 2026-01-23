library(terra)
aff<-rast('/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Afforestation_Clip/Landcover1900_affClip.tif')
ref<-rast('/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Reforestation_Clip/Landcover1900_refClip.tif')

aff2 <- classify(aff, rcl = matrix(c(54, 77, 1), ncol = 3, byrow = TRUE))
ref2 <- classify(ref, rcl = matrix(c(43, 44, 1), ncol = 3, byrow = TRUE))
continent<-vect('/Users/michunrong/Library/Mobile Documents/com~apple~CloudDocs/Downloads/ArcGISLayer/Country_Continent/continentsEckert.shp')



# 提取 aff2 中每个大洲内值为 1 的像元总数
aff_sum <- extract(aff2, continent, fun = sum, na.rm = TRUE)
ref_sum <- extract(ref2, continent, fun = sum, na.rm = TRUE)

# 合并结果
result <- data.frame(
  CONTINENT = continent$CONTINENT,
  Afforestation = aff_sum[, 2],  # 第2列是统计值
  Reforestation = ref_sum[, 2]
)

result$Total<-continent$AreaEckert
result<-result[-8,]


library(dplyr)
result2 <- result %>%
  mutate(
    CONTINENT = ifelse(CONTINENT %in% c("Oceania", "Australia"), "Oceania", CONTINENT)
  ) %>%
  group_by(CONTINENT) %>%
  summarise(
    Afforestation = sum(Afforestation, na.rm = TRUE),
    Reforestation = sum(Reforestation, na.rm = TRUE),
    Total = sum(Total, na.rm = TRUE)
  ) %>%
  ungroup()


result2$AffPer<-round(result2$Afforestation/result2$Total*100,1)
result2$RefPer<-round(result2$Reforestation/result2$Total*100,1)
result2$ForestationPer<-round((result2$Afforestation+result2$Reforestation)/result2$Total*100,1)

write.csv(result2,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Appendix/ForestationArea_InContinent.csv",row.names = F)
