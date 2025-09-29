rm(list = ls())
library(openxlsx)
data<-read.xlsx("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/crop&pasture_Originalvegetation.xlsx")

library(ggplot2)



p<-ggplot(subset(data,OriginalLandcover!="NoData"),aes(x=OriginalLandcover,y=Percent,fill=OriginalLandcover))+
   geom_bar(stat = "identity",width = 0.6)+
  xlab("")+
  ylab("Percent")+
   facet_wrap(~class)+
  theme_bw()+
  theme(
    strip.text.x = element_text(size = 20, colour = "Black"),
    axis.title =element_text(size=16,color = "black"), ####????????
    axis.text.y  =element_text(size=12,colour = "black"),
    axis.text.x  =element_blank(),
   # axis.text.x  =element_text(size=12,colour = "black",angle = 45,vjust = 0.5),
    legend.text  =element_text(size=12,colour = "black"))
 
p

ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig_crop&pasture_OriginalVegetation.pdf",plot = p,
       width = 8.5,height = 3,units = "in")

