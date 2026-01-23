rm(list = ls())
library(dplyr)
library(tidyverse)
library(stringr)


habLegend<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/habitats_legend2.csv")
habLegend<-habLegend[,c("Code","Habitat2")]
LandLegend<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Appendix/Landcover_Code.csv")
##########Amphibian 
df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats3.csv")

df<-merge(df,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df<-unique(df)


library(tidyverse)
df2<-  df %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)


# ʹ??group_by??summarize????????
df_combined <- df2 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))
write.csv(df_combined,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine2_1.csv",row.names = F)




df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))

write.csv(df_combined3,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine3_1.csv",row.names = F)

####Reptile

df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats3.csv")

df<-merge(df,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df<-unique(df)
df2<-  df %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)

# ʹ??group_by??summarize????????
df_combined <- df2 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))
write.csv(df_combined,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine2_1.csv",row.names = F)

df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))

write.csv(df_combined3,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine3_1.csv",row.names = F)



##########Mammal
df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats3.csv")
df<-merge(df,habLegend,by.x="code",by.y="Code",all.x=TRUE)
df<-unique(df)
df2<-  df %>% 
  distinct(scientificName, Habitat2, .keep_all = TRUE)


# ʹ??group_by??summarize????????
df_combined <- df2 %>%
  group_by(scientificName) %>%
  summarize(value = paste(Habitat2, collapse = ", "))
write.csv(df_combined,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine2_1.csv",row.names = F)


df_combined3 <- df_combined %>%
  mutate(code = sapply(value, function(x) {
    codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
    codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
    paste(codes_mapped, collapse = ", ")
  }))

write.csv(df_combined3,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine3_1.csv",row.names = F)


##########Bird
  df<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats3.csv")
  df<-merge(df,habLegend,by.x="code",by.y="Code",all.x=TRUE)
  df<-unique(df)
  df2<-  df %>% 
    distinct(scientificName, Habitat2, .keep_all = TRUE)
  
  # ʹ??group_by??summarize????????
  df_combined <- df2 %>%
    group_by(scientificName) %>%
    summarize(value = paste(Habitat2, collapse = ", "))
  write.csv(df_combined,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine2_1.csv",row.names = F)
  
  df_combined3 <- df_combined %>%
    mutate(code = sapply(value, function(x) {
      codes <- str_extract_all(x, paste(LandLegend$label, collapse = "|"))[[1]]
      codes_mapped <- LandLegend$code[match(codes, LandLegend$label)]
      paste(codes_mapped, collapse = ", ")
    }))
  
  write.csv(df_combined3,file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine3_1.csv",row.names = F)
  



