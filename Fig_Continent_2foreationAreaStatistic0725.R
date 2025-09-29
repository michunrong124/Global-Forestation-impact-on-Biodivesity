rm(list = ls())
library(openxlsx)
library(ggplot2)



######3Cate 
library(openxlsx)
data<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/continent_3Forestation_Area_3Foresttype1225.csv")

library(ggplot2)
data$Continent<-factor(data$Continent,levels = c("North America","Europe","Asia","South America","Africa","Oceania"))
#data$landcover<-factor(data$landcover,levels = c("cropland","unmanaged grass/shrubland","pasture/rangeland","sparse/no vegetation","water","forest"))
data$Forestation<-factor(data$Forestation,levels = c("Afforestation","Reforestation"),
                         labels = c("Aff","Ref"))

data$landcover<-factor(data$landcover,levels = c("Agroforestry","Plantation","Natural"))
#color<-c("#FCB332","#52BB7A","#F9EF69","#DCDCDC","#03C5E5","#176533")
#color<-c("#93E5AE","#FCB332","#176533")
color<-c("#5F6DB3","#894C9E","#176533")


p<-ggplot(data,aes(x=Forestation,y=CellCount/1000000,,group=landcover,fill=landcover))+
  #geom_bar(stat = "identity",width = 0.55)+
  geom_col(width = 0.5,alpha=0.9)+
  xlab("")+
  ylab("Foresation areas million km2")+
  theme_bw()+
  theme(
    strip.text.x = element_text(size = 20, colour = "Black"),
    axis.title =element_text(size=16,color = "black"), ####????????
    axis.text.y  =element_text(size=12,colour = "black"),
    #axis.text.x  =element_blank(),
    axis.text.x  =element_text(size=16,colour = "black"),#face = "italic"),
    legend.text  =element_text(size=12,colour = "black"))+
  scale_fill_manual (values = color)+
  facet_wrap(~Continent,scales = "free")
p

ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig_Continent_3Forestation_AreaStatistic_3ForestType_0725.pdf",plot = p,
       width = 9,height = 6,units = "in")



####
library(dplyr)
head(data)

summary_data <- data %>%
  group_by(landcover) %>%
  summarise(total_cellcount = sum(CellCount, na.rm = TRUE)) %>%
  mutate(percentage = total_cellcount / sum(total_cellcount) * 100)

print(summary_data)


# A tibble: 3 × 3
#landcover    total_cellcount percentage
#<fct>                  <int>      <dbl>
#1 Agroforestry          513997       13.9
#2 Plantation            476443       12.9
#3 Natural              2716478       73.3
