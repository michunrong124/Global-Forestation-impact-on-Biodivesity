###Calculate Winner, Loser and Stable Species Number Statistic
######@Mcr  20250903
rm(list = ls())
library(parallel)
library(dplyr)

groups<-c("Amphibian","Reptile","Bird","Mammal")
forest_types<-c("Natural","Plantation","Agroforestry")

myfunction<-function(group,forest_type) {
  Aff<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/Statistic_Result/",group,"_Afforestation_RangeChangeTrend_Significant_",forest_type,".csv"))
  Ref<-read.csv(paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/Statistic_Result/",group,"_Reforestation_RangeChangeTrend_Significant_",forest_type,".csv"))
  
  n_Aff<-nrow(Aff)
  n_Ref<-nrow(Ref)
  
  Aff$category <- ifelse(Aff$change > 0 & Aff$p.value <= 0.05/n_Aff, "Winner",
                         ifelse(Aff$change < 0 & Aff$p.value <= 0.05/n_Aff, "Loser",
                                "Stable"))
  write.csv(Aff,file = paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/Statistic_Result/",group,"_Afforestation_RangeChangeTrend_Significant_",forest_type,"AddWinner.csv"),row.names = F)
  
  
  Aff_table <- data.frame(table(Aff$category))
  Aff_table$Class<-"Afforestation"
  
  
  Ref$category <- ifelse(Ref$change > 0 & Ref$p.value <= 0.05/n_Ref, "Winner",
                         ifelse(Ref$change < 0 & Ref$p.value <= 0.05/n_Ref, "Loser",
                                "Stable"))
  write.csv(Ref,file = paste0("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/Statistic_Result/",group,"_Reforestation_RangeChangeTrend_Significant_",forest_type,"AddWinner.csv"),row.names = F)
  Ref_table <- data.frame(table(Ref$category))
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
names(All)[1:2]<-c("Trend","spenum")



library(ggplot2)

All$Group<-factor(All$Group,levels = c("Amphibian","Reptile","Bird","Mammal"))
All$Class<-factor(All$Class,levels = c("Afforestation","Reforestation"),
                  labels = c("Afforestation","Reforestation"))
All$foresttype<-factor(All$foresttype,levels = c("Natural","Plantation","Agroforestry"))

color<-c("#E73547","grey80","#3176B3")





# 计算 spenum 总和，并计算每个值的百分比
All2 <- All %>%
  group_by(Class, foresttype, Trend) %>%
  summarise(total_spenum = sum(spenum)) %>%
  mutate(percent = total_spenum / sum(total_spenum) * 100) %>%
  ungroup() %>%
  arrange(Class, foresttype, Trend)




####
library(reshape2)
library(dplyr)
head(All2)
summary_data <- All2 %>%
  group_by(Class, Trend) %>%
  summarise(total_spenum2 = sum(total_spenum, na.rm = TRUE)) %>%
  ungroup()


print(summary_data)

#summary_data2 <- reshape2::dcast(summary_data,
#                                     Class ~ Trend,
#                                     value.var = "total_spenum2")

summary_data2<-dcast(summary_data, Class ~ Trend)

rownames(summary_data2)<-summary_data2[,1]
summary_data2<-summary_data2[,-1]
chi_result<-chisq.test(summary_data2)

print(chi_result)
chi_result$stdres

###Winner
round(summary_data2[1,3]/ rowSums(summary_data2[1,])*100,1) # Winner Aff
round(summary_data2[2,3]/ rowSums(summary_data2[2,])*100,1) # Winner Ref

###Loser
round(summary_data2[1,1]/ rowSums(summary_data2[1,])*100,1) ##Loser Aff
round(summary_data2[2,1]/ rowSums(summary_data2[2,])*100,1) ##Loser Ref

###NoChange
round(summary_data2[1,2]/ rowSums(summary_data2[1,])*100,1) ##NoChange Aff
round(summary_data2[2,2]/ rowSums(summary_data2[2,])*100,1) ##NoChange Ref



summary_data3<-summary_data2
summary_data3$Total<-rowSums(summary_data3)
summary_data3Win<-summary_data3[,c(3,4)]
summary_data3Los<-summary_data3[,c(1,4)]
summary_data3NoC<-summary_data3[,c(2,4)]

chi_result<-chisq.test(summary_data3Win)
print(chi_result)
chi_result$stdres


chi_result<-chisq.test(summary_data3NoC)
print(chi_result)
chi_result$stdres

chi_result<-chisq.test(summary_data3Los)
print(chi_result)
chi_result$stdres



summary_data <-subset(All2,foresttype=="Agroforestry") %>%
  group_by(Class,Trend) %>%
  summarise(total_spenum2 = sum(total_spenum, na.rm = TRUE)) 

print(summary_data)

summary_data2<-dcast(summary_data, Class ~ Trend)

rownames(summary_data2)<-summary_data2[,1]
summary_data2<-summary_data2[,-1]
chi_result<-chisq.test(summary_data2)

print(chi_result)
chi_result$stdres


summary_data <-All2 %>%
  group_by(foresttype,Trend) %>%
  summarise(total_spenum2 = sum(total_spenum, na.rm = TRUE)) 

print(summary_data)

summary_data2<-dcast(summary_data, foresttype ~ Trend)


rownames(summary_data2)<-summary_data2[,1]
summary_data2<-summary_data2[,-1]
chi_result<-chisq.test(summary_data2)

print(chi_result)
chi_result$stdres


summary_dataAff <-subset(All2,Class=="Afforestation") %>%
  group_by(foresttype,Trend) %>%
  summarise(total_spenum2 = sum(total_spenum, na.rm = TRUE)) 

print(summary_dataAff)
summary_dataAff2<-dcast(summary_dataAff, foresttype ~ Trend)
rownames(summary_dataAff2)<-summary_dataAff2[,1]
summary_dataAff2<-summary_dataAff2[,-1]
chi_resultAff<-chisq.test(summary_dataAff2)

print(chi_resultAff)
chi_resultAff$stdres



summary_dataRef <-subset(All2,Class=="Reforestation") %>%
  group_by(foresttype,Trend) %>%
  summarise(total_spenum2 = sum(total_spenum, na.rm = TRUE)) 

print(summary_dataRef)
summary_dataRef2<-dcast(summary_dataRef, foresttype ~ Trend)
rownames(summary_dataRef2)<-summary_dataRef2[,1]
summary_dataRef2<-summary_dataRef2[,-1]
chi_resultRef<-chisq.test(summary_dataRef2)

print(chi_resultRef)
chi_resultRef$stdres

# 计算 spenum 总和，并计算每个值的百分比
All3 <- All %>%
  group_by(Group, Class, foresttype) %>%
  mutate(percent = spenum / sum(spenum) * 100)


All2$Group<-"All"
All2<-All2[,c("Trend","Class","Group","foresttype","percent","total_spenum")]
All3<-All3[,c("Trend","Class","Group","foresttype","percent","spenum")]
names(All2)<-names(All3)
All4<-rbind(All2,All3)

All4$Group<-factor(All4$Group,levels = c("All","Amphibian","Reptile","Bird","Mammal"))



winner_loser_ratio <- All4 %>%
  group_by(Class,Group,foresttype) %>%
  summarise(
    Winner = sum(spenum[Trend == "Winner"]),
    Loser = sum(spenum[Trend == "Loser"]),
    .groups = "drop"
  ) %>%
  mutate(Winner_Loser_Ratio = Winner / Loser)


pratio <- ggplot(winner_loser_ratio, aes(x = foresttype, y = Winner_Loser_Ratio)) +
  geom_col(position = "stack", width = 0.4,fill="grey55") +  # ???? stack ?????????? Trend
  
  ylab("Ratio of Winner/Loser") +  # ???? y ??????
  xlab("") +  # ???? x ??????
  #facet_grid(Group ~  Class+foresttype, scales = "free") + 
  facet_grid(Group ~ Class, scales = "free_y") +
  geom_text(aes(label = ifelse(Winner_Loser_Ratio > 0.01, round(Winner_Loser_Ratio, 2), "")),
            vjust = -0.5, size = 3)+
  
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

pratio

ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/Fig_Loser&Winner_5*2_All_Group_Ratio.pdf",plot = pratio,
       width = 7,height = 9,units = "in")
