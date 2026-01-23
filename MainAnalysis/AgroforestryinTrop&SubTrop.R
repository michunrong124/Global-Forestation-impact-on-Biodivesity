library(terra)
a<-rast("/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Appendix/ForestTypes2020/global_2020.tif")
land<-vect('/Users/michunrong/Library/Mobile Documents/com~apple~CloudDocs/Downloads/ArcGISLayer/地形要素矢量图/ne_10m_landEckert.shp')

a2<-project(a,land,method='near')

a3<-crop(a2,land)
a4<-mask(a3,land)
#writeRaster(a4,filename = "/Users/michunrong/Documents/Google/Paper/Nature-BasedClimateSolution/Afforestation/Appendix/ForestTypes2020/global_2020Eckert4.tif",overwrite=TRUE)

dat<-freq(a4)

library(terra)

# 1. 在“经纬度”（longlat）坐标系中建 ±35° 的带状多边形
trop_sub <- vect(rbind(
  c(-180, -35),
  c( 180, -35),
  c( 180,  35),
  c(-180,  35),
  c(-180, -35)
),
type = "polygons",
crs  = "+proj=longlat +datum=WGS84 +no_defs"   # 这里不用 EPSG:4326，而是直接给 proj4 字符串
)

# 2. 投影到 a4 的 CRS（Eckert）
trop_sub_eck <- project(trop_sub, a4)

# 3. 在 Eckert 投影下提取热带+亚热带区域
a_trop <- mask(a4, trop_sub_eck)

# 4. 统计 value==6 的像元数并求占比
f_all  <- freq(a4)
f_trop <- freq(a_trop)

n_all  <- dat[dat[,"value"]  == 6, "count"]
n_trop <- f_trop[f_trop[,"value"] == 6, "count"]

p <- n_trop / n_all   
p   ###0.9141085



