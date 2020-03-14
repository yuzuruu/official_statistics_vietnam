# Official statistics by General Statistics Office (GSO) of Vietnam
# 14th. March 2020
# Yuzuru Utsunomiya


# ---- read.library ----
library(tidyverse)
library(sf)
library(ggmap)
library(viridis)
library(viridisLite)
#
##
### --- END --- ###

# ---- read.data ----
mekong.delta.province.name <- c("An Giang", "Bạc Liêu", "Bến Tre", "Cần Thơ", "Cà Mau", "Đồng Tháp", "Hậu Giang", "Kiên Giang", "Long An", "Sóc Trăng", "Tiền Giang", "Trà Vinh", "Vĩnh Long")
mekong.delta.province.gid <- c("VNM.1_1", "VNM.2_1", "VNM.6_1", "VNM.12_1", "VNM.13_1", "VNM.18_1", "VNM.24_1", "VNM.33_1", "VNM.39_1", "VNM.51_1", "VNM.58_1", "VNM.59_1", "VNM.61_1")
mekong.delta.district <- c("An Phu", "Cho Moi", "Chau Doc", "Chau Phu", "Chau Thanh", "Long Xuyen", "Phu Tan", "Tinh Bien", "Tan Chau", "Thoai Son", "Tri Ton", "Bac Lieu", "Dong Hai", "Gia Rai", "Hong Dan", "Hoa Binh", "Phuoc Long", "Vinh Loi", "Ben Tre", "Ba Tri", "Binh Dai", "Cho Lach", "Chau Thanh", "Giong Trom", "Mo Cay Bac", "Mo Cay Nam", "Thanh Phu", "Binh Thuy", "Co Do", "Cai Rang", "Ninh Kieu", "O Mon", "Phong Dien", "Thot Not", "Thoi Lai", "Vinh Thanh", "Ca Mau", "Cai Nuoc", "Dam Doi", "Nam Can", "Ngoc Hien", "Phu Tan", "Thoi Binh", "Tran Van Thoi", "U Minh", "Cao Lanh", "Cao Lanh", "Chau Thanh", "Hong Ngu", "Hong Ngu", "Lap Vo", "Lai Vung", "Sa Dec", "Tam Nong", "Tan Hong", "Thanh Binh", "Thap Muoi", "Chau Thanh", "Chau Thanh A", "Long My", "Long My", "Nga Bay", "Phung Hiep", "Vi Thanh", "Vi Thuy", "An Bien", "An Minh", "Chau Thanh", "Giong Rieng", "Giang Thanh", "Go Quao", "Ha Tien", "Hon Dat", "Kien Hai", "Kien Luong", "Phu Quoc", "Rach Gia", "Tan Hiep", "U Minh Thuong", "Vinh Thuan", "Ben Luc", "Can Duoc", "Can Giuoc", "Chau Thanh", "Duc Hoa", "Duc Hue", "Kien Tuong", "Moc Hoa", "Tan An", "Tan Hung", "Tan Thanh", "Tan Tru", "Thanh Hoa", "Thu Thua", "Vinh Hung", "Chau Thanh", "Cu Lao Dung", "Ke Sach", "Long Phu", "My Tu", "My Xuyen", "Nga Nam", "Soc Trang", "Thanh Tri", "Tran De", "Vinh Chau", "Cai Be", "Cai Lay", "Cai Lay", "Cho Gao", "Chau Thanh", "Go Cong", "Go Cong Dong", "Go Cong Tay", "My Tho", "Tan Phu Dong", "Tan Phuoc", "Cau Ke", "Cau Ngang", "Cang Long", "Chau Thanh", "Duyen Hai", "Duyen Hai", "Tieu Can", "Tra Cu", "Tra Vinh", "Binh Minh", "Binh Tan", "Long Ho", "Mang Thit", "Tam Binh", "Tra On", "Vinh Long", "Vung Liem")
# population in 2017
mekong.delta.population <- 
  readxl::read_excel("data_district_mekong_delta.xlsx",
                     sheet = "population",
                     range = "A1:F135",
                     col_names = TRUE
                     ) %>% 
  dplyr::select(GID_1, 
                population.2017
                )
# read map data and add some valuables
mekong.delta.map <- 
  base::readRDS("./administrative_boundaries/gadm36_VNM_2_sf.rds") %>% 
  dplyr::filter(GID_1 %in% mekong.delta.province.gid) %>% 
  dplyr::mutate(area = sf::st_area(.)) %>% 
  dplyr::left_join(mekong.delta.population, 
                   by = "GID_1"
                   ) %>% 
  dplyr::mutate(population.density.2017 = as.numeric(as.character(1000000*population.2017 / area)))

# draw a base map by Google
# provide information of google API key
source("../scattered_item/map.key.r")
# center of satellite imagery map
# This locates Mo O cafeteria
lat.center.mekong.delta <- c(9.75)
lon.center.mekong.delta <- c(105.25) #106.1801492
# Obtain satellite imagery
mekong.delta.sat.01 <- 
  get_map(location = c(lon = lon.center.mekong.delta,
                       lat = lat.center.mekong.delta
  ), 
  # maptype = "satellite",
  maptype = "roadmap",
  zoom = 8
  ) %>% 
  ggmap()
#
##
### --- END --- ###

# ---- map.population.density ----
# overlay the maps together
mekong.delta.map.population.density.2017 <- 
  mekong.delta.sat.01 +  
  # coord_sf(crs = st_crs(3857)) + 
  geom_sf(
    data = mekong.delta.map,
    aes(fill = population.density.2017
        ),
    colour = "white",
    size = 0.05,
    inherit.aes = FALSE
          ) +
  scale_fill_viridis_c(
    trans = "sqrt",
    option = "plasma",
    alpha = 0.10,
    direction = 1
    ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  xlim(104.5, 107) +
  ylim(8.5, 11) +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Population density \n (Unit: Persons / km^2)",
       caption = "\U00a9 Google"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.8,0.2),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 8)
    ) + 
  # adjust scalebar's preferences
  ggsn::scalebar(x.min = 105.10,
                 x.max = 106.10,
                 y.min = 8.56,
                 y.max = 8.60, 
                 dist_unit = "km",
                 dist = 50, 
                 st.size = 4,
                 st.dist = 1.0,
                 height = 0.5,
                 model = "WGS84", 
                 transform = TRUE,
                 location = "bottomright",
                 box.fill = c("grey30", "white"), # left and right
                 box.color = "white",
                 st.color = "grey60"
  ) 
#
##
### --- END --- ###

# ---- save.map.01 ----
# save the map
# Comment out when not in use
ggsave("mekong.delta.map.population.density.2017.pdf",
       plot = mekong.delta.map.population.density.2017
       )
#
##
### --- END --- ###







