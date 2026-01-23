################Merge IUCN species distribution list with habitat list
#####################@Mcr 20240618
rm(list = ls())
AmpDis<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/SpeciesDistribution/AmphibianDist_nameList.csv")
AmpHab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine2.csv")
Amp<-unique(merge(AmpDis,AmpHab,by.x="Species",by.y="scientificName",all.x=TRUE))

RepDis<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/SpeciesDistribution/ReptileDist_nameList.csv")
RepHab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine2.csv")
Rep<-unique(merge(RepDis,RepHab,by.x="Species",by.y="scientificName",all.x=TRUE))



BirdDis<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/SpeciesDistribution/BirdDist_nameList.csv")
BirdHab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine2.csv")
Bird<-unique(merge(BirdDis,BirdHab,by.x="Species",by.y="scientificName",all.x=TRUE))



MamDis<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/SpeciesDistribution/MammalDist_nameList.csv")
MamHab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine2.csv")
Mam<-unique(merge(MamDis,MamHab,by.x="Species",by.y="scientificName",all.x=TRUE))
