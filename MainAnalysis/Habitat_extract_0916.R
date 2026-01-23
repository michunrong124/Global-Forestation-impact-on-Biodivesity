##########Habitat Transfor#####
#######Mcr 20240916 in Robersten Hall
rm(list = ls())

library(dplyr)
library(tidyverse)
library(stringr)

habLegend<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/habitats_legend3.csv")
habLegend<-habLegend[,c("Code","Habitat2")]
LandLegend<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Appendix/Landcover_Code3.csv")



###########Amphibian
df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats.csv")
df<-subset(df,suitability=="Suitable")
df2 <- df[df$code %in% habLegend$Code, ]

write.csv(df2,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats2.csv",row.names = F)

df2<-df2[,c("scientificName", "code")]


df3<-merge(df2,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df3<-unique(df3)

library(tidyverse)
df4<-  df3 %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)

# ????group_by??summarize????????
df_combined <- df4 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))



df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))


Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/Elevation.csv")
df_combined4<-merge(df_combined3,Ele,by="scientificName",all.x=TRUE)
write.csv(df_combined4,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine6_1.csv",row.names = F)



###########Reptile
df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats.csv")
df<-subset(df,suitability=="Suitable")
df2 <- df[df$code %in% habLegend$Code, ]

write.csv(df2,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats2.csv",row.names = F)

df2<-df2[,c("scientificName", "code")]


df3<-merge(df2,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df3<-unique(df3)

library(tidyverse)
df4<-  df3 %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)

# ????group_by??summarize????????
df_combined <- df4 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))



df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))


Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/Elevation.csv")
df_combined4<-merge(df_combined3,Ele,by="scientificName",all.x=TRUE)
write.csv(df_combined4,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine6_1.csv",row.names = F)


###########Bird
df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats.csv")
df<-subset(df,suitability=="Suitable")
df2 <- df[df$code %in% habLegend$Code, ]

write.csv(df2,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats2.csv",row.names = F)

df2<-df2[,c("scientificName", "code")]


df3<-merge(df2,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df3<-unique(df3)

library(tidyverse)
df4<-  df3 %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)

# ????group_by??summarize????????
df_combined <- df4 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))



df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))


Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/Elevation.csv")
df_combined4<-merge(df_combined3,Ele,by="scientificName",all.x=TRUE)
df_combined4$scientificName <- gsub(" ", "_", df_combined4$scientificName)
write.csv(df_combined4,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine6_1.csv",row.names = F)





###########Mammal
df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats.csv")
df<-subset(df,suitability=="Suitable")
df2 <- df[df$code %in% habLegend$Code, ]

write.csv(df2,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats2.csv",row.names = F)

df2<-df2[,c("scientificName", "code")]


df3<-merge(df2,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df3<-unique(df3)

library(tidyverse)
df4<-  df3 %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)

# ????group_by??summarize????????
df_combined <- df4 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))



df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))


Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/Elevation.csv")
df_combined4<-merge(df_combined3,Ele,by="scientificName",all.x=TRUE)
write.csv(df_combined4,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine6_1.csv",row.names = F)




