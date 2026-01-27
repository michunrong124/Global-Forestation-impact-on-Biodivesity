###Calculate Winner, Loser and Stable Species Number Statistic
######@Mcr  20250903


rm(list = ls())
library(parallel)

groups<-c("Amphibian","Reptile","Bird","Mammal")
forest_types<-c("Natural","Plantation","Agroforestry")

myfunction<-function(group,forest_type) {
  Aff<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/Statistic_Result/",group,"_Afforestation_RangeChangeTrend_Significant_",forest_type,"AddWinner.csv"))
  Ref<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/Statistic_Result/",group,"_Reforestation_RangeChangeTrend_Significant_",forest_type,"AddWinner.csv"))
  
  
  
  Aff_table <- data.frame(table(Aff$category,Aff$redlistCategory))
  Aff_table$Class<-"Afforestation"
  
  
  Ref_table <- data.frame(table(Ref$category,Ref$redlistCategory))
  Ref_table$Class<-"Reforestation"
  
  result<-rbind(Aff_table,Ref_table)
  result$Group<-group
  result$foresttype<-forest_type
  return(result)
}

result2 <- mclapply(1:length(groups), function(i) {
  lapply(1:length(forest_types), function(j) {
    myfunction(groups[i], forest_types[j])
  })
}, mc.cores = 4)

result3 <- do.call(rbind, do.call(c, result2))

All<-result3
names(All)[1:3]<-c("Trend","redlistCategory","spenum")

#write.csv(All,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Statistic_Result/All_RangeChangeTrend_Significant_AddWinner.csv",row.names = F)



library(ggplot2)

All$Group<-factor(All$Group,levels = c("Amphibian","Reptile","Bird","Mammal"))
All$Class<-factor(All$Class,levels = c("Afforestation","Reforestation"),
                  labels = c("Afforestation","Reforestation"))
All$foresttype<-factor(All$foresttype,levels = c("Natural","Plantation","Agroforestry"))

All$redlistCategory<-factor(All$redlistCategory,levels = c("Critically Endangered","Endangered","Vulnerable",
                                                           "Near Threatened","Least Concern","Data Deficient"))
All$Trend<-factor(All$Trend,levels=c("Loser","Stable","Winner"))
#color<-c("#E73547","grey80","#3176B3")
# Load necessary libraries
library(dplyr)

# 计算 spenum 总和，并计算每个值的百分比
All2 <- All %>%
  group_by(Class, foresttype, redlistCategory,Trend) %>%
  summarise(total_spenum = sum(spenum)) %>%
  mutate(percent = total_spenum / sum(total_spenum) * 100) %>%
  ungroup() %>%
  arrange(Class, foresttype,redlistCategory,Trend)

All2$redlistCategory<-factor(All2$redlistCategory,levels = c("Critically Endangered","Endangered","Vulnerable",
                                                             "Near Threatened","Least Concern","Data Deficient"),
                             labels =  c("CR","EN","VU","NT","LC","DD"))

All2$Trend<-factor(All2$Trend,levels=c("Loser","Stable","Winner"))
names(All2)[5]<-"spenum"


All22<- All2 %>%
  group_by(Class,  foresttype, redlistCategory) %>%
  mutate(total_spenum= sum(spenum),  # ???????? redlistCategory ????????????
         percent = spenum/ total_spenum * 100) %>%
  ungroup()



All3<- All %>%
  group_by(Class, Group, foresttype, redlistCategory) %>%
  mutate(total_spenum= sum(spenum),  # ???????? redlistCategory ????????????
         percent = spenum/ total_spenum * 100) %>%
  ungroup()

All3$redlistCategory<-factor(All3$redlistCategory,levels = c("Critically Endangered","Endangered","Vulnerable",
                                                             "Near Threatened","Least Concern","Data Deficient"),
                             labels =  c("CR","EN","VU","NT","LC","DD"))

All3$Trend<-factor(All3$Trend,levels=c("Loser","Stable","Winner"))



All22$Group<-"All"

All23<-All22[,c("Trend", "redlistCategory","Class","Group","foresttype","spenum","percent")]
All3<-All3[,c("Trend", "redlistCategory","Class","Group","foresttype","spenum","percent")]

All4<-rbind(All23,All3)


#All4$redlistCategory<-factor(All4$redlistCategory,levels = c("Critically Endangered","Endangered","Vulnerable",
#                                                             "Near Threatened","Least Concern","Data Deficient"),
#                             labels =  c("CR","EN","VU","NT","LC","DD"))

All4$Trend<-factor(All4$Trend,levels=c("Loser","Stable","Winner"))

All4$Group<-factor(All4$Group,levels = c("All","Amphibian","Reptile","Bird","Mammal"))

winner_loser_ratio <- All4 %>%
  group_by(Class,Group, foresttype, redlistCategory) %>%
  summarise(
    Winner = sum(spenum[Trend == "Winner"]),
    Loser = sum(spenum[Trend == "Loser"]),
    .groups = "drop"
  ) %>%
  mutate(Winner_Loser_Ratio = Winner / Loser)


winner_loser_ratio2<-subset(winner_loser_ratio,redlistCategory!="DD")


p4ratio <- ggplot(subset(winner_loser_ratio2), aes(x = redlistCategory, y = Winner_Loser_Ratio)) +
  geom_col(position = "stack", width = 0.8,fill="grey50") +  # ???? stack ?????????? Trend
  
  ylab("Percentage of specis") +  # ???? y ??????
  xlab("") +  # ???? x ??????
  #facet_grid(Group ~  Class+foresttype, scales = "free") + 
  facet_grid(Group ~ Class + foresttype, scales = "free_y")+
  geom_text(aes(label = ifelse(Winner_Loser_Ratio > 0.01, round(Winner_Loser_Ratio, 2), "")),
            vjust = -0.5, size = 2.5)+
  
  # ?? foresttype??Group ?? Class ?????????????? foresttype ???м???
  #scale_fill_brewer(palette = "Set1") +  # ???? Set1 ??????
  #scale_fill_manual(values=color)+
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 16, colour = "black"),
    strip.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"),
    axis.text.y  = element_text(size = 16, colour = "black"),
    axis.text.x  = element_text(size = 15, colour = "black", angle = 45, hjust = 1),
    legend.text  = element_text(size = 15, colour = "black")
  )

p4ratio



