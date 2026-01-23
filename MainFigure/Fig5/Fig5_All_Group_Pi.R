library("terra")
Aff<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/All_Afforestation_SRchange.tif")
Ref<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/All_Reforestation_SRchange.tif")


reclass_matrix <- matrix(c(
  400, Inf, 1000,      # ???? 400
  200, 400, 400,      # 200 - 400
  100, 200, 200,      # 100 - 200
  0.00001, 100, 100,        # 1 - 100
  0, 0, 0,          # 0
  -100, -0.00001, -1, 
  -200, -100, -100,# -100 - -1
  -400, -200, -200,     # -400 - -200
  -Inf, -400, -400), 
  ncol = 3, byrow = TRUE)

# ִ???ط???
reclassified_Aff <- classify(Aff, rcl = reclass_matrix)
AffFreq<-freq(reclassified_Aff)

AffFreq$Forestation_type<-"Afforestation"


reclassified_Ref <- classify(Ref, rcl = reclass_matrix)
RefFreq<-freq(reclassified_Ref)
RefFreq$Forestation_type<-"Reforestation"

Freq<-rbind(AffFreq,RefFreq)
Freq$value<-as.factor(Freq$value)

library(ggplot2)

p<-ggplot(Freq,)

custom_colors <- c(
  "-400" = alpha("#D32F2F", 0.7),
  "-200" = alpha("#F57C00", 0.7),
  "-100" = alpha("#FBC02D", 0.7),
  "-1" = alpha("#388E3C", 0.7),
  "0" = alpha("#1976D2", 0.7),
  "100" = alpha("#7B1FA2", 0.7),
  "200" = alpha("#0288D1", 0.7)
)

# ??????ɫ??Ӧ?? 70% ͸????
color <- alpha(c("#7D1415", "#EE2024", "#F67F21", "#F6EB14", "#99CB6F", "#6FCCDE", "#4378BC", "#3953A5", "#2A2F80"), 0.9)

color<-c("#7D1415","#EE2024","#F67F21","#F6EB14","#99CB6F","#6FCCDE","#4378BC","#3953A5","#2A2F80")
# ???ر?Ҫ?İ?
library(ggplot2)
library(dplyr)


total_counts <- Freq %>%
  group_by(Forestation_type) %>%
  summarise(total = sum(count))

Freq <- Freq %>%
  left_join(total_counts, by = "Forestation_type") %>%
  mutate(percentage = count / total * 100)
library(ggplot2)


p<-ggplot(Freq, aes(x = "", y = -percentage, fill = factor(value))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Forestation_type,ncol=1) +
  labs(fill = "Value", y = "Percentage (%)") +
  theme_void() +  # ȥ??????????????
 # ggtitle("Percentage Distribution of Values by Forestation Type")+
  geom_text(aes(label = paste0(round(percentage, 0))), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color)

p
ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/AllGroup_Pi.pdf",plot =p,
       units = "in",height = 5,width = 3)



