rm(list = ls())
library(terra)
library(parallel)
library(dplyr)

# ?????ļ???·??
input_folder <- "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/all_athromes_Landcover/Reclassified"
####https://public.yoda.uu.nl/geo/UU01/67UHB4.html
# ??ȡ????դ???ļ?
file_list <- list.files(input_folder, pattern = "\\.asc$", full.names = TRUE)

# ??ȡʱ????Ϣ??????
file_info <- data.frame(
  file_path = file_list,
  file_name = basename(file_list),
  # ??ȡ???ֲ???
  year = as.numeric(stringr::str_extract(basename(file_list), "\\d+")),
  era = ifelse(grepl("BC", file_list), "BC", "AD")
)

# ???���Ԫǰ??????Ϊ??ֵ????Ԫ????????Ϊ??ֵ
file_info <- file_info %>%
  mutate(year = ifelse(era == "BC", -year, year)) %>%
  arrange(year)  # ??ʱ??????

# ??ӡ????
print(file_info)


# ???????¸?ֵ??

#Builtup ????Ϊ 1
#Cropland ????Ϊ 2
#Pasture ????Ϊ 3
#Grassland ????Ϊ 4
#Forest ????Ϊ 5
#Desert/Tundra ????Ϊ 6
#Ice/Snow ????Ϊ 7
#NoData ????Ϊ 0

analyze_forest_transition <- function(file_info) {
  # ??ȡ2020?????��?????դ????Ϊ????????
  final_raster <- rast(file_info$file_path[file_info$year == 2020])
  forest_code <- 5  # ????ɭ?ֵĴ???Ϊ5
  
  # ??ʼ??????դ?񣨻???2020????դ????
  pre_forest_type <- final_raster
  pre_forest_year <- final_raster
  
  # ???ó?ʼֵΪ NA
  values(pre_forest_type) <- NA
  values(pre_forest_year) <- NA
  
  # ?Ӻ???ǰ????ʱ??????
  for (i in seq(nrow(file_info) - 1, 1, by = -1)) {
    # ??ǰ????
    current_year <- file_info$year[i]
    
    # ??ȡ??ǰդ??
    current_raster <- rast(file_info$file_path[i])
    
    # ?ҵ???Ԫ????ǰ?겻??ɭ?֣?2020????ɭ?֣?????δ????¼
    transitioning_cells <- which(
      values(final_raster) == forest_code &  # 2020????ɭ??
        values(current_raster) != forest_code &  # ??ǰ?겻??ɭ??
        is.na(values(pre_forest_type))  # ??δ??¼
    )
    
    # ???½???դ??
    if (length(transitioning_cells) > 0) {
      values(pre_forest_type)[transitioning_cells] <- values(current_raster)[transitioning_cells]
      values(pre_forest_year)[transitioning_cells] <- current_year
    }
  }
  
  # ???ؽ???
  list(pre_forest_type = pre_forest_type, pre_forest_year = pre_forest_year)
}



# ִ?з???
results <- analyze_forest_transition(file_info)

# ??ȡ????
pre_forest_type_raster <- results$pre_forest_type
pre_forest_year_raster <- results$pre_forest_year

# ???ƽ???
plot(pre_forest_type_raster, main = "Landcover Type Before Forest (2020)")
plot(pre_forest_year_raster, main = "Year of Transition to Forest (2020)")

forestation<-pre_forest_type_raster
earliest_year_file <- file_info$file_path[1]
earliest_raster <- rast(earliest_year_file)
earliest_raster2<-mask(earliest_raster,forestation)
Aff<-ifel(earliest_raster2==7,1,NA)
Ref<-ifel(earliest_raster2==5,1,NA)

print(freq(Aff)[1,3]*85)
print(freq(Ref)[1,3]*85)



Affyear<-mask(pre_forest_year_raster,Aff)
Refyear<-mask(pre_forest_year_raster,Ref)



# ͳ?? Affyear ?????ݺ?????
affyear_values <- as.data.frame(freq(Affyear, digits = 0))[,2:3]  # ??ȡֵ??Ƶ??
colnames(affyear_values) <- c("Year", "PixelCount")  # ????????

affyear_values <- affyear_values[!is.na(affyear_values$Year), ]

# ͳ?? Refyear ?????ݺ?????
refyear_values <- as.data.frame(freq(Refyear, digits = 0))[,2:3]  # ??ȡֵ??Ƶ??
colnames(refyear_values) <- c("Year", "PixelCount")  # ????????
refyear_values <- refyear_values[!is.na(refyear_values$Year), ]

# ????????
print("Afforestation:")
print(affyear_values)

print("Reforestation:")
print(refyear_values)


# ??????????ʶ
affyear_values$Type <- "Afforestation"
refyear_values$Type <- "Reforestation"

# ?ϲ?????
combined_values <- rbind(affyear_values, refyear_values)

library(ggplot2)
# ???ƶԱ?ͼ
p<-ggplot(combined_values, aes(x = Year, y = PixelCount, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Afforestation and Reforestation Area by Year", 
       x = "Year", y = "Area") +
  geom_vline(xintercept = 1900, color = "red", linetype = "dashed", size = 0.5) +  # ???Ӵ?ֱ??
  theme_bw() +
  scale_fill_manual(values = c("blue", "green"))

p

ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/ForestationHappen_InHistory.pdf", plot = p,
       units = "in",height = 3,width = 8)



library(dplyr)

# 假设你的两个数据框为 affyear_values 和 refyear_values

# 先合并数据
merged_df <- full_join(affyear_values, refyear_values, by = "Year", suffix = c("_aff", "_ref"))

# 计算总像素数量（对 NA 处理为 0）
merged_df <- merged_df %>%
  mutate(
    PixelCount_aff = ifelse(is.na(PixelCount_aff), 0, PixelCount_aff),
    PixelCount_ref = ifelse(is.na(PixelCount_ref), 0, PixelCount_ref),
    TotalPixelCount = PixelCount_aff + PixelCount_ref
  )

# 选择你需要的列
result <- merged_df %>%
  select(Year, TotalPixelCount)


colors<-"#3DA004"
pall<-ggplot(result, aes(x = Year, y = TotalPixelCount*10*10)) +
  geom_bar(stat = "identity", position = "dodge",color=colors) +
  labs(title = "Forestation Area by Year", 
       x = "Year", y = "Area (km2)") +
  geom_vline(xintercept = 1900, color = "red", linetype = "dashed", size = 0.5) +  # ???Ӵ?ֱ??
  theme_bw() #+
  #scale_fill_manual(values = c("green"))

pall

ggsave("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Figure/Appendix/ForestationHappen_InHistory_All.pdf", plot = pall,
       units = "in",height = 3,width = 7)




print(sum(subset(affyear_values,Year>=1900)$PixelCount)/sum(affyear_values$PixelCount)*100)

print(sum(subset(refyear_values,Year>=1900)$PixelCount)/sum(refyear_values$PixelCount)*100)


print(sum(subset(affyear_values,Year>=1960)$PixelCount)/sum(affyear_values$PixelCount)*100)

print(sum(subset(refyear_values,Year>=1960)$PixelCount)/sum(refyear_values$PixelCount)*100)


print(sum(subset(affyear_values,Year>=1990)$PixelCount)/sum(affyear_values$PixelCount)*100)

print(sum(subset(refyear_values,Year>=1990)$PixelCount)/sum(refyear_values$PixelCount)*100)


print(sum(subset(affyear_values,Year>=2000)$PixelCount)/sum(affyear_values$PixelCount)*100)
print(sum(subset(refyear_values,Year>=2000)$PixelCount)/sum(refyear_values$PixelCount)*100)

forest<-rbind(affyear_values,refyear_values)

print(sum(subset(forest,Year>=2000)$PixelCount)/sum(forest$PixelCount)*100)  ###93.02055
print(sum(subset(forest,Year>=1990)$PixelCount)/sum(forest$PixelCount)*100)### 55.00215
print(sum(subset(forest,Year>=1900)$PixelCount)/sum(forest$PixelCount)*100) ###36.72384
