library(dplyr)
groups<-c("Amphibian","Reptile","Bird","Mammal")
forest_types<-c("Natural","Plantation","Agroforestry")


# 设置文件路径
path <- "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Afforestation_SpeciesArea/Statistic_Result/"

# 初始化空数据框用于合并
Aff <- data.frame()

# 遍历所有 group 和 forest_type 组合
for (group in groups) {
  for (forest_type in forest_types) {
    # 生成文件名匹配
    file_pattern <- paste0(group, "_Afforestation_RangeChangeTrend_Significant_", forest_type, "\\.csv$")
    
    # 获取匹配的文件
    files <- list.files(path, pattern = file_pattern, full.names = TRUE)
    
    # 读取并合并数据
    if (length(files) > 0) {  # 确保至少有一个匹配文件
      temp_data <- do.call(rbind, lapply(files, read.csv))
      
      # 添加 group 和 forest_type 作为新列，以保留信息
      temp_data$Group <- group
      temp_data$ForestType <- forest_type
      
      # 合并数据
      Aff <- bind_rows(Aff, temp_data)
    }
  }
}

# 查看合并后的数据
head(Aff)

# 根据 species 列去重
Aff <- Aff %>% distinct(species, .keep_all = TRUE)

table(Aff$Group)


######Reforestation
rm(list = ls())

library(dplyr)
groups<-c("Amphibian","Reptile","Bird","Mammal")
forest_types<-c("Natural","Plantation","Agroforestry")


# 设置文件路径
path <- "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Table/Reforestation_SpeciesArea/Statistic_Result/"

# 初始化空数据框用于合并
Ref <- data.frame()

# 遍历所有 group 和 forest_type 组合
for (group in groups) {
  for (forest_type in forest_types) {
    # 生成文件名匹配
    file_pattern <- paste0(group, "_Reforestation_RangeChangeTrend_Significant_", forest_type, "\\.csv$")
    
    # 获取匹配的文件
    files <- list.files(path, pattern = file_pattern, full.names = TRUE)
    
    # 读取并合并数据
    if (length(files) > 0) {  # 确保至少有一个匹配文件
      temp_data <- do.call(rbind, lapply(files, read.csv))
      
      # 添加 group 和 forest_type 作为新列，以保留信息
      temp_data$Group <- group
      temp_data$ForestType <- forest_type
      
      # 合并数据
      Ref <- bind_rows(Ref, temp_data)
    }
  }
}

# 查看合并后的数据
head(Ref)

# 根据 species 列去重
Ref <- Ref %>% distinct(species, .keep_all = TRUE)

table(Ref$Group)




