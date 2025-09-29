rm(list = ls())
library(plotly)
library(openxlsx)

library(htmlwidgets)
library(webshot)

data<-read.xlsx("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Aff&Reforestaton_LandcoverChangeOriginal-Middle-Final_1225.xlsx",sheet = 1)

# 
unique_lables <- unique(c(data$source_land))

#labels <- c(paste0(unique_lables," Original"),
#            paste0(unique_lables," 2015"))
#labels<-c("cropland Original","pasture/rangeland Original","unmanaged grass/shrubland Original","sparse/no vegetation Original","water Original",
#          "cropland Middle","pasture/rangeland Middle","forest Middle","sparse/no vegetation Middle","unmanaged grass/shrubland Middle","forest")


labels<-c("grassland Original","sparse Original","water Original",
          "cropland Middle","pasture Middle","grassland Middle","water Middle",
          "natural forest Final","plantation forest Final","agroforestry Final")


# 
#colors <- c('rgba(209, 54, 45, 1)','rgba(243, 193, 122, 1)', ###urban, cropland, pasture
#            'rgba(251, 237, 78, 1)', 'rgba(85, 158, 60, 1)', 'rgba(186, 221, 146, 1)', 'rgba(191, 191, 191, 1)',
#            'rgba(209, 54, 45, 1)','rgba(243, 193, 122, 1)', ###urban, cropland, pasture
#            'rgba(251, 237, 78, 1)', 'rgba(85, 158, 60, 1)')

####urban:'rgba(209, 54, 45, 1)',cropland: 'rgba(243, 193, 122, 1)' pasture: 'rgba(251, 237, 78, 1)'
####forest:'rgba(251, 237, 78, 1)', unmanaged grass/shrubland: 'rgba(85, 158, 60, 1)', sparse/no vegetation: 'rgba(186, 221, 146, 1)', 'rgba(191, 191, 191, 1)'

#colors <- c("#3153A5","#C90936","#52BB7A","#F9EF69","#FCB332",  #"#6B8E23"
#            "#52BB7A","#FCB332","#F9EF69","#DCDCDC","#03C5E5"). #"black",

#link_colors0 <- c('rgba(0, 0, 0, 0.7)','rgba(201, 9, 54, 0.7)', 'rgba(51, 84, 165, 0.7)','rgba(83, 188, 122, 0.7)','rgba(247, 239, 108, 0.7)',
#                  'rgba(23, 101, 51, 0.7)','rgba(252, 179, 47, 0.7)','rgba(220, 220, 220, 0.7)','rgba(51, 84, 165, 0.7)')

#link_colors0<- c("#FCB332","#F9EF69","#DCDCDC","#52BB7A","#03C5E5","#DCDCDC") #"#6B8E23",
link_colors0<- c("#FCB332","#F9EF69","#52BB7A","#DCDCDC","#03C5E5") #"#6B8E23",
link_colors0<-adjustcolor(link_colors0, alpha.f = 0.7)
#link_colors0<- c("#FCB332","#F9EF69","#DCDCDC","#52BB7A","#03C5E5","#DCDCDC") #"#6B8E23",




source <- list()
target <- list()
value <- list()
link_colors <- list()
link_colors2<-list()

source<-data$source
target<-data$target
value<-data$count
#tt<-cbind(source,target,value)
link_colors2<-link_colors0[data$link_color+1]
link_colors<-link_colors2
#node_color<- c("#FCB332","#F9EF69","#52BB7A","#03C5E5","#DCDCDC", 
#               "#6B8E23","#DCDCDC","#52BB7A","#6B8E23")

node_color<- c("#52BB7A","#DCDCDC", "#03C5E5",
               "#FCB332", "#F9EF69","#52BB7A","#DCDCDC","#03C5E5",
               "#6B8E23","#894C9E","#5F6DB3")



# make Sankey figure
fig <- plot_ly(
  type = "sankey",
  orientation = 'v',
  node = list(
    pad = 15,
    thickness = 20,
    line = list(color = 'black', width = 0.5),
    label = labels,
    color = node_color,
    shape = "circular"  # Add this line to make the nodes circular
    ),
  link = list(
    source = unlist(source),
    target = unlist(target),
    value = unlist(value),
    color = unlist(link_colors)
  )
)

fig

a<-aggregate(count ~ source_land, data = data, sum)
a$total<-sum(a$count)/2
a$Percent<-round(a$count/a$total*100,1)
a<-aggregate(count ~ target_land, data = data, sum)
a$total<-sum(a$count)/2
a$Percent<-round(a$count/a$total*100,1)
saveWidget(fig, file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_SankeyDiagram_EckertOriginal-Middle-Final_Afforestation_v_1225.html")
# Convert HTML to PDF
webshot("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_SankeyDiagram_EckertOriginal-Middle-Final_Afforestation_v_1225.html", 
        file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_SankeyDiagram_EckertOriginal-Middle-Final_Afforestation_v_1225.pdf", delay = 3,vwidth = 800,vheight = 450)  # delay ensures all scripts are executed



#setwd("/Users/michunrong")
#orca(fig, file = "Global_SankeyDiagram_Eckert.pdf",width = 450,height=800)


##############Forstation



##############Reforstation

rm(list = ls())
library(plotly)
library(openxlsx)
library(htmlwidgets)
library(webshot)

data<-read.xlsx("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Aff&Reforestaton_LandcoverChangeOriginal-Middle-Final_1225.xlsx",sheet = 2)

# ????Ψһ??ǩ?б?
unique_lables <- unique(c(data$source_land))

#labels <- c(paste0(unique_lables," Original"),
#            paste0(unique_lables," 2015"))
#labels<-c("cropland Original","pasture/rangeland Original","unmanaged grass/shrubland Original","sparse/no vegetation Original","water Original",
#          "cropland Middle","pasture/rangeland Middle","forest Middle","sparse/no vegetation Middle","unmanaged grass/shrubland Middle","forest")


labels<-c("natural forest Original",
          "cropland Middle","pasture Middle","grassland Middle","sparse Middle",
          "natural forest Final","plantation forest Final","agroforestry Final")


# ????ÿ??Ŀ???ڵ?????ɫ
#colors <- c('rgba(209, 54, 45, 1)','rgba(243, 193, 122, 1)', ###urban, cropland, pasture
#            'rgba(251, 237, 78, 1)', 'rgba(85, 158, 60, 1)', 'rgba(186, 221, 146, 1)', 'rgba(191, 191, 191, 1)',
#            'rgba(209, 54, 45, 1)','rgba(243, 193, 122, 1)', ###urban, cropland, pasture
#            'rgba(251, 237, 78, 1)', 'rgba(85, 158, 60, 1)')

####urban:'rgba(209, 54, 45, 1)',cropland: 'rgba(243, 193, 122, 1)' pasture: 'rgba(251, 237, 78, 1)'
####forest:'rgba(251, 237, 78, 1)', unmanaged grass/shrubland: 'rgba(85, 158, 60, 1)', sparse/no vegetation: 'rgba(186, 221, 146, 1)', 'rgba(191, 191, 191, 1)'

#colors <- c("#3153A5","#C90936","#52BB7A","#F9EF69","#FCB332",  #"#6B8E23"
#            "#52BB7A","#FCB332","#F9EF69","#DCDCDC","#03C5E5"). #"black",

#link_colors0 <- c('rgba(0, 0, 0, 0.7)','rgba(201, 9, 54, 0.7)', 'rgba(51, 84, 165, 0.7)','rgba(83, 188, 122, 0.7)','rgba(247, 239, 108, 0.7)',
#                  'rgba(23, 101, 51, 0.7)','rgba(252, 179, 47, 0.7)','rgba(220, 220, 220, 0.7)','rgba(51, 84, 165, 0.7)')

#link_colors0<- c("#FCB332","#F9EF69","#DCDCDC","#52BB7A","#03C5E5","#DCDCDC") #"#6B8E23",
link_colors0<- c("#FCB332","#F9EF69","#52BB7A","#DCDCDC") #"#6B8E23",
link_colors0<-adjustcolor(link_colors0, alpha.f = 0.7)



source <- list()
target <- list()
value <- list()
link_colors <- list()
link_colors2<-list()

source<-data$source
target<-data$target
value<-data$count
#tt<-cbind(source,target,value)
link_colors2<-link_colors0[data$link_color+1]
link_colors<-link_colors2
#node_color<- c("#FCB332","#F9EF69","#52BB7A","#03C5E5","#DCDCDC", 
#               "#6B8E23","#DCDCDC","#52BB7A","#6B8E23")

node_color<- c("#6B8E23",
               "#FCB332", "#F9EF69","#52BB7A","#DCDCDC",
               "#6B8E23","#894C9E","#5F6DB3")




fig <- plot_ly(
  type = "sankey",
  orientation = 'v',
  node = list(
    pad = 15,
    thickness = 20,
    line = list(color = 'black', width = 0.5),
    label = labels,
    color = node_color,
    shape = "circular"  # Add this line to make the nodes circular
  ),
  link = list(
    source = unlist(source),
    target = unlist(target),
    value = unlist(value),
    color = unlist(link_colors)
  )
)

fig

a<-aggregate(count ~ source_land, data = data, sum)
a$total<-sum(a$count)/2
a$Percent<-round(a$count/a$total*100,1)

a<-aggregate(count ~ target_land, data = data, sum)
a$total<-sum(a$count)/2
a$Percent<-round(a$count/a$total*100,1)
saveWidget(fig, file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_SankeyDiagram_EckertOriginal-Middle-Final_Reorestation_v_1225.html")
# Convert HTML to PDF
webshot("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_SankeyDiagram_EckertOriginal-Middle-Final_Reorestation_v_1225.html", 
        file = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_SankeyDiagram_EckertOriginal-Middle-Final_Reforestation_v_1225.pdf", delay = 3,vwidth = 800,vheight = 450)  # delay ensures all scripts are executed



#setwd("/Users/michunrong")
#orca(fig, file = "Global_SankeyDiagram_Eckert.pdf",width = 450,height=800)


