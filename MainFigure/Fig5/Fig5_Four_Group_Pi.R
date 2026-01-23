##################20241030
###############@Mcr Roberston Hall
rm(list = ls())
library("terra")



AmphibianAff<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Amphibian_Afforestation_SRchange.tif")
AmphibianRef<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Amphibian_Reforestation_SRchange.tif")


Amphibian_matrix <- matrix(c(
  50, Inf, 1000,      # ???? 400
  20, 50, 50,      # 100 - 200
  0.00001, 20, 20,        # 1 - 100
  0, 0, 0,          # 0
  -20, -0.00001, -1, 
  -Inf, -20, -20), 
  ncol = 3, byrow = TRUE)

# ִ???ط???
AmphibianAff2 <- classify(AmphibianAff, rcl = Amphibian_matrix)
AmphibianAffFreq<-freq(AmphibianAff2)
AmphibianAffFreq$Forestation_type<-"Afforestation"
AmphibianAffFreq$Group<-"Amphibian"



AmphibianRef2 <- classify(AmphibianRef, rcl = Amphibian_matrix)
AmphibianRefFreq<-freq(AmphibianRef2)
AmphibianRefFreq$Forestation_type<-"Reforestation"
AmphibianRefFreq$Group<-"Amphibian"

AmphibianFreq<-rbind(AmphibianAffFreq,AmphibianRefFreq)
AmphibianFreq$value<-as.factor(AmphibianFreq$value)

library(ggplot2)


# ??????ɫ??Ӧ?? 70% ͸????
color <- alpha(c("#7D1415",  "#F67F21",  "#99CB6F", "#6FCCDE", "#4378BC", "#2A2F80"), 0.9)

#color<-c("#7D1415","#EE2024","#F67F21","#F6EB14","#99CB6F","#6FCCDE","#4378BC","#3953A5","#2A2F80")
# ???ر?Ҫ?İ?
library(ggplot2)
library(dplyr)


total_counts <- AmphibianFreq %>%
  group_by(Forestation_type) %>%
  summarise(total = sum(count))

AmphibianFreq <- AmphibianFreq %>%
  left_join(total_counts, by = "Forestation_type") %>%
  mutate(percentage = count / total * 100)


pAmphibian<-ggplot(AmphibianFreq, aes(x = "", y = -percentage, fill = factor(value))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Forestation_type,nrow=1) +
  labs(fill = "Value", y = "Percentage (%)") +
  theme_void() +  # ȥ??????????????
 # ggtitle("Percentage Distribution of Values by Forestation Type")+
  geom_text(aes(label = paste0(round(percentage, 0))), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color)
pAmphibian


ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/AllGroup_Pi.pdf",plot =p,
       units = "in",height = 5,width = 3)



############Reptile
ReptileAff<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Reptile_Afforestation_SRchange.tif")
ReptileRef<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Reptile_Reforestation_SRchange.tif")


Reptile_matrix <- matrix(c(
  60, Inf, 1000,      # ???? 400
  30, 60, 60,      # 100 - 200
  0.00001, 30, 30,        # 1 - 100
  0, 0, 0,          # 0
  -40, -0.00001, -1, 
  -Inf, -40, -40), 
  ncol = 3, byrow = TRUE)

# ִ???ط???
ReptileAff2 <- classify(ReptileAff, rcl = Reptile_matrix)
ReptileAffFreq<-freq(ReptileAff2)
ReptileAffFreq$Forestation_type<-"Afforestation"
ReptileAffFreq$Group<-"Reptile"

ReptileRef2 <- classify(ReptileRef, rcl = Reptile_matrix)
ReptileRefFreq<-freq(ReptileRef2)
ReptileRefFreq$Forestation_type<-"Reforestation"
ReptileRefFreq$Group<-"Reptile"

ReptileFreq<-rbind(ReptileAffFreq,ReptileRefFreq)
ReptileFreq$value<-as.factor(ReptileFreq$value)

library(ggplot2)


# ??????ɫ??Ӧ?? 70% ͸????
color <- alpha(c("#7D1415",  "#F67F21",  "#99CB6F", "#6FCCDE", "#4378BC", "#2A2F80"), 0.9)

#color<-c("#7D1415","#EE2024","#F67F21","#F6EB14","#99CB6F","#6FCCDE","#4378BC","#3953A5","#2A2F80")
# ???ر?Ҫ?İ?
library(ggplot2)
library(dplyr)


total_counts <- ReptileFreq %>%
  group_by(Forestation_type) %>%
  summarise(total = sum(count))

ReptileFreq <- ReptileFreq %>%
  left_join(total_counts, by = "Forestation_type") %>%
  mutate(percentage = count / total * 100)


pReptile<-ggplot(ReptileFreq, aes(x = "", y = -percentage, fill = factor(value))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Forestation_type,nrow=1) +
  labs(fill = "Value", y = "Percentage (%)") +
  theme_void() +  # ȥ??????????????
  # ggtitle("Percentage Distribution of Values by Forestation Type")+
  geom_text(aes(label = paste0(round(percentage, 0))), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color)
pReptile




###########Bird
BirdAff<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Bird_Afforestation_SRchange.tif")
BirdRef<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Bird_Reforestation_SRchange.tif")


Bird_matrix <- matrix(c(
  200, Inf, 1000,      # ???? 400
  100, 200, 200,      # 100 - 200
  0.00001, 100, 100,        # 1 - 100
  0, 0, 0,          # 0
  -150, -0.00001, -1, 
  -Inf, -150, -150), 
  ncol = 3, byrow = TRUE)

# ִ???ط???
BirdAff2 <- classify(BirdAff, rcl = Bird_matrix)
BirdAffFreq<-freq(BirdAff2)
BirdAffFreq$Forestation_type<-"Afforestation"
BirdAffFreq$Group<-"Bird"


BirdRef2 <- classify(BirdRef, rcl = Bird_matrix)
BirdRefFreq<-freq(BirdRef2)
BirdRefFreq$Forestation_type<-"Reforestation"
BirdRefFreq$Group<-"Bird"

BirdFreq<-rbind(BirdAffFreq,BirdRefFreq)
BirdFreq$value<-as.factor(BirdFreq$value)

library(ggplot2)


# ??????ɫ??Ӧ?? 70% ͸????
color <- alpha(c("#7D1415",  "#F67F21",  "#99CB6F", "#6FCCDE", "#4378BC", "#2A2F80"), 0.9)

#color<-c("#7D1415","#EE2024","#F67F21","#F6EB14","#99CB6F","#6FCCDE","#4378BC","#3953A5","#2A2F80")
# ???ر?Ҫ?İ?
library(ggplot2)
library(dplyr)


total_counts <- BirdFreq %>%
  group_by(Forestation_type) %>%
  summarise(total = sum(count))

BirdFreq <- BirdFreq %>%
  left_join(total_counts, by = "Forestation_type") %>%
  mutate(percentage = count / total * 100)


pBird<-ggplot(BirdFreq, aes(x = "", y = -percentage, fill = factor(value))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Forestation_type,nrow=1) +
  labs(fill = "Value", y = "Percentage (%)") +
  theme_void() +  # ȥ??????????????
  # ggtitle("Percentage Distribution of Values by Forestation Type")+
  geom_text(aes(label = paste0(round(percentage, 0))), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color)
pBird




########Mammal

MammalAff<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Mammal_Afforestation_SRchange.tif")
MammalRef<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/Mammal_Reforestation_SRchange.tif")


Mammal_matrix <- matrix(c(
  80, Inf, 1000,      # ???? 400
  40, 80, 80,      # 100 - 200
  0.00001, 40, 40,        # 1 - 100
  0, 0, 0,          # 0
  -40, -0.00001, -1, 
  -Inf, -40, -40), 
  ncol = 3, byrow = TRUE)

# ִ???ط???
MammalAff2 <- classify(MammalAff, rcl = Mammal_matrix)
MammalAffFreq<-freq(MammalAff2)
MammalAffFreq$Forestation_type<-"Afforestation"
MammalAffFreq$Group<-"Mammal"

MammalRef2 <- classify(MammalRef, rcl = Mammal_matrix)
MammalRefFreq<-freq(MammalRef2)
MammalRefFreq$Forestation_type<-"Reforestation"
MammalRefFreq$Group<-"Mammal"

MammalFreq<-rbind(MammalAffFreq,MammalRefFreq)
MammalFreq$value<-as.factor(MammalFreq$value)

library(ggplot2)


# ??????ɫ??Ӧ?? 70% ͸????
color <- alpha(c("#7D1415",  "#F67F21",  "#99CB6F", "#6FCCDE", "#4378BC", "#2A2F80"), 0.9)

#color<-c("#7D1415","#EE2024","#F67F21","#F6EB14","#99CB6F","#6FCCDE","#4378BC","#3953A5","#2A2F80")
# ???ر?Ҫ?İ?
library(ggplot2)
library(dplyr)


total_counts <- MammalFreq %>%
  group_by(Forestation_type) %>%
  summarise(total = sum(count))

MammalFreq <- MammalFreq %>%
  left_join(total_counts, by = "Forestation_type") %>%
  mutate(percentage = count / total * 100)


pMammal<-ggplot(MammalFreq, aes(x = "", y = -percentage, fill = factor(value))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Forestation_type,nrow=1) +
  labs(fill = "Value", y = "Percentage (%)") +
  theme_void() +  # ȥ??????????????
  # ggtitle("Percentage Distribution of Values by Forestation Type")+
  geom_text(aes(label = paste0(round(percentage, 0))), 
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = color)
pMammal


library(gridExtra)

p<-grid.arrange(pAmphibian,pReptile,pBird,pMammal,nrow=4)

ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig5/FourGroup_Pi.pdf",plot =p,
       units = "in",height = 20,width = 5)

