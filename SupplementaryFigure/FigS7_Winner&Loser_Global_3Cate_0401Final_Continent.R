###Calculate Winner, Loser and Stable Species Number Statistic
######@Mcr  20250401
library(parallel)

rm(list = ls())

groups<-c("Amphibian","Reptile","Bird","Mammal")
forest_types<-c("Natural","Plantation","Agroforestry")
Continents<-c("Europe","Africa","Asia","Oceania","North America","South America")


myfunction <- function(Continent, group, forest_type) {
  Aff <- read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Statistic_Result/Continent/Afforestation/", group, "_Afforestation_RangeChangeTrend_Significant_", Continent, "_", forest_type, ".csv"))
  Ref <- read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Statistic_Result/Continent/Reforestation/", group, "_Reforestation_RangeChangeTrend_Significant_", Continent, "_", forest_type, ".csv"))
  
  n_Aff <- nrow(Aff)
  n_Ref <- nrow(Ref)
  
  Aff$category <- ifelse(Aff$change > 0 & Aff$p.value <= 0.05/n_Aff, "Winner",
                         ifelse(Aff$change < 0 & Aff$p.value <= 0.05/n_Aff, "Loser", "Stable"))
  Aff_table <- data.frame(table(Aff$category))
  Aff_table$Class <- "Afforestation"
  
  
  
  Ref$category <- ifelse(Ref$change > 0 & Ref$p.value <= 0.05/n_Ref, "Winner",
                         ifelse(Ref$change < 0 & Ref$p.value <= 0.05/n_Ref, "Loser", "Stable"))
  Ref_table <- data.frame(table(Ref$category))
  Ref_table$Class <- "Reforestation"
  
  result <- rbind(Aff_table, Ref_table)
  result$Group <- group
  result$foresttype <- forest_type
  result$Continent <- Continent
  return(result)
}

# 使用 mclapply 并确保所有结果都收集到 result2 中
result2 <- mclapply(1:length(Continents), function(i) {
  continent_results <- mclapply(1:length(groups), function(k) {
    group_results <- lapply(1:length(forest_types), function(j) {
      myfunction(Continents[i], groups[k], forest_types[j])
    })
    do.call(rbind, group_results)
  }, mc.cores = 4)
  do.call(rbind, continent_results)
}, mc.cores = 4)

# 将最终结果组合成一个数据框
result3 <- do.call(rbind, result2)


All<-result3
names(All)[1:2]<-c("Trend","spenum")


library(ggplot2)

All$Group<-factor(All$Group,levels = c("Amphibian","Reptile","Bird","Mammal"))
All$Class<-factor(All$Class,levels = c("Afforestation","Reforestation"),
                                  labels = c("Afforestation","Reforestation"))
All$foresttype<-factor(All$foresttype,levels = c("Natural","Plantation","Agroforestry"))

color<-c("#E73547","grey80","#3176B3")



# 计算 spenum 总和，并计算每个值的百分比
All3 <- All %>%
  group_by(Continent,Group, Class, foresttype) %>%
  mutate(percent = spenum / sum(spenum) * 100)


# 计算 spenum 总和，并计算每个值的百分比
All3 <- All %>%
  group_by(Continent,Class, foresttype, Trend) %>%
  summarise(total_spenum = sum(spenum)) %>%
  mutate(percent = total_spenum / sum(total_spenum) * 100) %>%
  ungroup() %>%
  arrange(Class, foresttype, Trend)


All3$Group<-"All"

All3<-All3[,c(1,7,2:6)]

# 计算 spenum 总和，并计算每个值的百分比
All2 <- All %>%
  group_by(Continent,Group,Class, foresttype, Trend) %>%
  summarise(total_spenum = sum(spenum)) %>%
  mutate(percent = total_spenum / sum(total_spenum) * 100) %>%
  ungroup() %>%
  arrange(Class, foresttype, Trend)




All4<-rbind(All3,All2)


All4$Group<-factor(All4$Group,levels = c("All","Amphibian","Reptile","Bird","Mammal"))
All4$Class<-factor(All4$Class,levels = c("Afforestation","Reforestation"),
                  labels = c("Afforestation","Reforestation"))
All4$foresttype<-factor(All4$foresttype,levels = c("Natural","Plantation","Agroforestry"))


for (Con in Continents){
  p4 <- ggplot(subset(All4,Continent == Con), aes(x = foresttype, y = total_spenum, group = Trend, fill = Trend)) +
    geom_col(width = 0.5) +
    geom_text(data = subset(All4, Continent == Con), aes(label = paste0(round(percent, 1)), color = Trend), 
              vjust = -4, size = 5) +
    
    ylab("Number of species") +
    xlab("") +
    title(Con)+
    scale_fill_manual(values = color) +
    facet_grid(Group~ Class, scales = "free_y") +
    theme_bw() +
    theme(
      strip.text.x = element_text(size = 18, colour = "Black"),
      strip.text.y = element_text(size = 18, colour = "Black"),
      axis.title.y = element_text(size = 16, colour = "black"),
      axis.text.y  = element_text(size = 16, colour = "black"),
      axis.text.x  = element_text(size = 16, colour = "black", angle = 45, hjust = 1),
      legend.text  = element_text(size = 16, colour = "black")
    )
  
  p4
  
  ggsave(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/Continent/Fig_Loser&Winner_5*2_Group_",Con,"_Num.pdf"),plot = p4,
         width = 8,height = 10,units = "in")
  
}






p<-ggplot(All,aes(x = foresttype,y=spenum,group=Trend,fill=Trend))+
  geom_col(width = 0.5)+
  ylab("Species number")+
  xlab("")+
  scale_fill_manual(values=color)+
  facet_wrap(Group~Class,scales = "free",nrow=4)+
  theme_bw()+
  theme(#legend.position = "none",
    strip.text.x = element_text(size = 18, colour = "Black"),
    # axis.title =element_text(size=14,color = "black"), ####????????
    axis.text.y  =element_text(size=12,colour = "black"),
    axis.text.x  =element_text(size=10,colour = "black",angle =45,hjust = 1),
    legend.text  =element_text(size=12,colour = "black"))

p


ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/StatisticResult/Fig_Loser&Winner.pdf",plot = p,
       width = 10,height = 3.2,units = "in")
