########Amphibian
rm(list = ls())
Hab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine3_1.csv")
Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/Elevation.csv")
Hab2<-merge(Hab,Ele,by="scientificName")
write.csv(Hab2,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine4_1.csv",row.names = F)

####Reptile
rm(list = ls())
Hab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine3_1.csv")
Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/Elevation.csv")
Hab2<-merge(Hab,Ele,by="scientificName")
write.csv(Hab2,file="/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine4_1.csv",row.names = F)


####Mammal
rm(list = ls())
Hab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine3_1.csv")
Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/Elevation.csv")
Hab2<-merge(Hab,Ele,by="scientificName")
write.csv(Hab2,file="/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine4_1.csv",row.names = F)




####Bird
rm(list = ls())
Hab<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine3_1.csv")
Ele<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/Elevation.csv")
Hab2<-merge(Hab,Ele,by="scientificName")
write.csv(Hab2,file="/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine4_1.csv",row.names = F)
Hab2$scientificName<-gsub(" ","_",Hab2$scientificName)
write.csv(Hab2,file="/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine4_2.csv",row.names = F)





########Amphibian
rm(list = ls())
Amp<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/habitats_combine4_1.csv")
Amplist<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/AmphibianUnsuitableHabitat_List.csv")
Amplist2<-merge(Amplist,Amp,by="scientificName")
write.csv(Amplist2,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Amphibian_search_Reasult/AmphibianUnsuitableHabitat_List2.csv",row.names = F)



Amp<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/habitats_combine4_1.csv")
Amplist<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/ReptileUnsuitableHabitat_List.csv")
Amplist2<-merge(Amplist,Amp,by="scientificName")
write.csv(Amplist2,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Reptile_search_Reasult/ReptileUnsuitableHabitat_List2.csv",row.names = F)


Amp<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/habitats_combine4_2.csv")
Amplist<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/BirdUnsuitableHabitat_List.csv")
Amplist2<-merge(Amplist,Amp,by="scientificName")
write.csv(Amplist2,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Bird_search_Reasult/BirdUnsuitableHabitat_List2.csv",row.names = F)



Amp<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/habitats_combine4_1.csv")
Amplist<-read.csv("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/MammalUnsuitableHabitat_List.csv")
Amplist2<-merge(Amplist,Amp,by="scientificName")
write.csv(Amplist2,"/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/VertebratesHabitat/IUCN_Mammal_search_Reasult/MammalUnsuitableHabitat_List2.csv",row.names = F)

