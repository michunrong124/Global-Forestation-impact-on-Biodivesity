#############
########Afforestation
rm(list = ls())
library(terra)
library(tidyr)
Land<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_Afforestation_ChangeFlow.tif")
Change<-data.frame(freq(Land))
names(Change)[2]<-"landcoverChange"
Change<-Change[,-1]

Change2 <- separate(Change, landcoverChange, into = c("Original", "Middle", "Final"), sep = "; ",remove = FALSE)

# 列出需要替换的类别名称和对应的替换值
replacements <- list("category 22" = "cropland",
                     "category 33" = "pasture",
                     "category 55" = "grassland",
                     "category 44" = "natural forest",
                     "category 45" = "plantation forest",
                     "category 46" = "agroforestry",
                     "category 66" = "sparse",
                     "category 77" = "water")

# 对分开的所有列进行替换操作
Change2[, c("Original", "Middle", "Final")] <- lapply(
  Change2[, c("Original", "Middle", "Final")],
  function(column) {
    for (key in names(replacements)) {
      column <- gsub(key, replacements[[key]], column)
    }
    return(column)
  }
)

Change3 <- Change2[rep(1:nrow(Change2), each = 2), ]
Change3$No. <- rep(c(1,2),)


Change3$source_land <- ifelse(Change3$No. == 1, 
                         paste(Change3$Original, "Original", sep = " "), 
                         paste(Change3$Middle, "Middle", sep = " "))

Change3$target_land <- ifelse(Change3$No. == 1, 
                         paste(Change3$Middle, "Middle", sep = " "), 
                         paste(Change3$Final, "Final", sep = " "))

# 查看结果
head(Change3)

# 假设你的数据框名为 Change3

# 为 Original 列中的每个唯一值创建一个从 0 开始的数字编码
middle_values <- unique(Change3$Middle)
middle_map <- setNames(0:(length(middle_values) - 1), middle_values)

# 创建 link_color 列，并根据 Middle列的值分配数字
Change3$link_color <- middle_map[Change3$Middle]

# 查看结果
head(Change3)




# 1. 提取唯一值并按照年份排序
unique_values <- unique(c(Change3$source_land, Change3$target_land))
# 按照年份进行排序：Original < Middle < Final
unique_values <- unique_values[order(grepl("Original", unique_values),
                                     grepl("Middle", unique_values),
                                     grepl("Final", unique_values),
                                     decreasing = TRUE)]

# 2. 创建从 0 开始的编码映射
value_map <- setNames(0:(length(unique_values) - 1), unique_values)

# 3. 创建 source 和 target 列
Change3$source <- value_map[Change3$source_land]
Change3$target <- value_map[Change3$target_land]

# 查看结果
head(Change3)

Afforestation<-Change3

a<-subset(Afforestation,No.==1)

######Reforestation

library(terra)
Land<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Fig2/Global_Reforestation_ChangeFlow.tif")
Change<-data.frame(freq(Land))
names(Change)[2]<-"landcoverChange"
Change<-Change[,-1]

Change2 <- separate(Change, landcoverChange, into = c("Original", "Middle", "Final"), sep = "; ",remove = FALSE)

# 列出需要替换的类别名称和对应的替换值
replacements <- list("category 22" = "cropland",
                     "category 33" = "pasture",
                     "category 55" = "grassland",
                     "category 44" = "natural forest",
                     "category 45" = "plantation forest",
                     "category 46" = "agroforestry",
                     "category 66" = "sparse",
                     "category 77" = "water")

# 对分开的所有列进行替换操作
Change2[, c("Original", "Middle", "Final")] <- lapply(
  Change2[, c("Original", "Middle", "Final")],
  function(column) {
    for (key in names(replacements)) {
      column <- gsub(key, replacements[[key]], column)
    }
    return(column)
  }
)

Change3 <- Change2[rep(1:nrow(Change2), each = 2), ]
Change3$No. <- rep(c(1,2),)


Change3$source_land <- ifelse(Change3$No. == 1, 
                              paste(Change3$Original, "Original", sep = " "), 
                              paste(Change3$Middle, "Middle", sep = " "))

Change3$target_land <- ifelse(Change3$No. == 1, 
                              paste(Change3$Middle, "Middle", sep = " "), 
                              paste(Change3$Final, "Final", sep = " "))

# 查看结果
head(Change3)

# 假设你的数据框名为 Change3

# 为 Middle 列中的每个唯一值创建一个从 0 开始的数字编码
middle_values <- unique(Change3$Middle)
middle_map <- setNames(0:(length(middle_values) - 1), middle_values)

# 创建 link_color 列，并根据 Middle列的值分配数字
Change3$link_color <- middle_map[Change3$Middle]

# 查看结果
head(Change3)




# 1. 提取唯一值并按照年份排序
unique_values <- unique(c(Change3$source_land, Change3$target_land))
# 按照年份进行排序：Original < Middle < Final
unique_values <- unique_values[order(grepl("Original", unique_values),
                                     grepl("Middle", unique_values),
                                     grepl("Final", unique_values),
                                     decreasing = TRUE)]

# 2. 创建从 0 开始的编码映射
value_map <- setNames(0:(length(unique_values) - 1), unique_values)

# 3. 创建 source 和 target 列
Change3$source <- value_map[Change3$source_land]
Change3$target <- value_map[Change3$target_land]

# 查看结果
head(Change3)

Reforestation<-Change3



################Export 
library(openxlsx)

# ????һ???µ? workbook
wb <- createWorkbook()
# ?? workbook ???ӹ???????????
addWorksheet(wb, "Afforestation")
writeData(wb, "Afforestation", Afforestation)

addWorksheet(wb, "Reforestation")
writeData(wb, "Reforestation", Reforestation)

saveWorkbook(wb, "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Aff&Reforestaton_LandcoverChangeOriginal-Middle-Final_1225.xlsx", overwrite = TRUE)



