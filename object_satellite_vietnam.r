#################################################################### 
# All about analyses using MS global ML building footprints
# 8th. June 2022
# Yuzuru Utsunomiya
# Source: 
# https://github.com/microsoft/GlobalMLBuildingFootprints#will-there-be-more-data-coming-for-other-geographies
# Projection: WGS84
# ETSG: 4326
# license: https://opendatacommons.org/licenses/odbl/
#################################################################### 


# ---- read.library ----
library(tidyverse)
library(sf)
library(furrr)
#
#
##
### END ---
##
#
# ---- read.data ----
# read object data
# object_Vietnam_data <- sf::st_read("./object/Vietnam.geojsonl")
# Remove objects when not in use
# rm(object_Vietnam)
# read shapefiles by country
shp_Vietnam <- 
  sf::st_read(
    "./shapefiles/shapefiles/VNM_adm2.shp", 
    options = "ENCODING=UTF-8", 
    stringsAsFactors=FALSE
  ) %>% 
  dplyr::mutate_if(
    is.character, 
    enc2utf8
  )
# ---- read.function ----
# a function find address from lat / lon
# We thank following links.
# https://qiita.com/nozma/items/808bce2f496eabd50ff1
# https://qiita.com/uri/items/69b2c05f7b3a21d3aad3
find_city <- function(sp_polygon = df, lon = lon, lat = lat){
  # find a polygon containing a certain pair of lon / lat
  which.row <- 
    sf::st_contains(
      sp_polygon, 
      sf::st_point(
        c(
          lon, 
          lat
        )
      ), 
      sparse = FALSE
    ) %>%  
    grep(TRUE, .)
  # If not, leave a warning message
  if (identical(which.row, integer(0)) == TRUE) {
    # message("指定した座標がポリゴンに含まれません")
    return(NA)
    # If exist, obtain information of coordinates
  } else {
    geos <- 
      sp_polygon[which.row, ] %>%
      # transform from factor to character
      dplyr::mutate_if(
        is.factor, 
        as.character
      ) %>% 
      # obtain necessary part of the shapefile
      dplyr::mutate_at(
        # dplyr::vars(NAME_1, NAME_2, NAME_3), 
        dplyr::vars(NAME_1), 
        dplyr::funs(
          dplyr::if_else(
            # Is it NA?
            condition = is.na(.),
            # if NA, return blank
            true = "", 
            # if not, use it
            false = .
          )
        )
      )
    # make a dataset of administrative boundaries
    # Names and IDs are obtained from shapefiles
    res <- tibble::data_frame(
      province_code = geos$ID_1,
      district_code = geos$ID_2,
      # town_code = geos$ID_3,
      province_name = geos$NAME_1,
      district_name = geos$VARNAME_2,
      # town_name = geos$NAME_3
    )
    # return results
    return(res)
  }
}
#
#
##
### END ---
##
#
# 
# ---- make.area.data ----
# # WARNING
# # This process needs long computation periods.
# object_Vietnam_lat_lon <-
#   # provide data
#   object_Vietnam_data%>%
#   # evaluate the geometries whether they are validated
#   # If not, functions below will not work.
#   # In detail, refer to the following page.
#   # https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using
#   dplyr::mutate(
#     true_false = sf::st_is_valid(.)
#     ) %>%
#   # select vali data
#   dplyr::filter(true_false == "TRUE") %>%
#   # add longitude and latitude
#   dplyr::mutate(
#     area = sf::st_area(.),
#     lon = st_coordinates(sf::st_centroid(.))[,1],
#     lat = st_coordinates(sf::st_centroid(.))[,2]
#     )
# saveRDS(object_Vietnam_lat_lon, "object_Vietnam_lat_lon.rds")
# rm(object_Vietnam_lat_lon)
# gc()
# gc()
# 
# read
object_Vietnam_sample <- 
  readRDS("object_Vietnam_lat_lon.rds") %>% 
  dplyr::mutate(
    id = c(1:nrow(.))) %>%
  dplyr::select(-true_false, -area) %>% 
  sf::st_drop_geometry()
# %>%
#   dplyr::sample_frac(size = 0.1)




# primitive but prudent way
set.seed(123)
# obtain group id
# NOTE
# We need to separate the data without surplus
# This time, our data can be divided by an appropriate number (16).
idx <- sample(1:nrow(object_Vietnam_sample)/27)
cv <- 
  split(
    idx, 
    ceiling(
      seq_along(idx) / floor(length(idx) / 27
      )
    )
  ) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  data.table::setnames(
    c(
      "group01","group02","group03","group04","group05","group06","group07","group08","group09","group10",
      "group11","group12","group13","group14","group15","group16","group17","group18","group19","group20",
      "group21","group22","group23","group24","group25","group26","group27"
      
    )
  ) %>% 
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "id"
  )
# combine the original data and randomly-allocated group
object_Vietnam_sample_group <- 
  object_Vietnam_sample %>% 
  left_join(
    cv, 
    by = "id"
  )



# obtain provinces' name by point
# 01
object_Vietnam_address_01 <- object_Vietnam_sample_group %>%filter(group == "group01") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_01,"object_Vietnam_address_01.rds")
gc()
gc()
# 02
object_Vietnam_address_02 <- object_Vietnam_sample_group %>%filter(group == "group02") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_02,"object_Vietnam_address_02.rds")
gc()
gc()
# 03
object_Vietnam_address_03 <- object_Vietnam_sample_group %>%filter(group == "group03") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_03,"object_Vietnam_address_03.rds")
gc()
gc()
# 04
object_Vietnam_address_04 <- object_Vietnam_sample_group %>%filter(group == "group04") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_04,"object_Vietnam_address_04.rds")
gc()
gc()
# 05
object_Vietnam_address_05 <- object_Vietnam_sample_group %>%filter(group == "group05") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_05,"object_Vietnam_address_05.rds")
gc()
gc()
# 06
object_Vietnam_address_06 <- object_Vietnam_sample_group %>%filter(group == "group06") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_06,"object_Vietnam_address_06.rds")
gc()
gc()
# 07
object_Vietnam_address_07 <- object_Vietnam_sample_group %>%filter(group == "group07") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_07,"object_Vietnam_address_07.rds")
gc()
gc()
# 08
object_Vietnam_address_08 <- object_Vietnam_sample_group %>%filter(group == "group08") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_08,"object_Vietnam_address_08.rds")
gc()
gc()
# 09
object_Vietnam_address_09 <- object_Vietnam_sample_group %>%filter(group == "group09") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_09,"object_Vietnam_address_09.rds")
gc()
gc()
# 10
object_Vietnam_address_10 <- object_Vietnam_sample_group %>%filter(group == "group10") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_10,"object_Vietnam_address_10.rds")
gc()
gc()
# 11
object_Vietnam_address_11 <- object_Vietnam_sample_group %>%filter(group == "group11") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_11,"object_Vietnam_address_11.rds")
gc()
gc()
# 12
object_Vietnam_address_12 <- object_Vietnam_sample_group %>%filter(group == "group12") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_12,"object_Vietnam_address_12.rds")
gc()
gc()
# 13
object_Vietnam_address_13 <- object_Vietnam_sample_group %>%filter(group == "group13") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_13,"object_Vietnam_address_13.rds")
gc()
gc()
# 14
object_Vietnam_address_14 <- object_Vietnam_sample_group %>%filter(group == "group14") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_14,"object_Vietnam_address_14.rds")
gc()
gc()
# 15
object_Vietnam_address_15 <- object_Vietnam_sample_group %>%filter(group == "group15") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_15,"object_Vietnam_address_15.rds")
gc()
gc()
# 16
object_Vietnam_address_16 <- object_Vietnam_sample_group %>%filter(group == "group16") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_16,"object_Vietnam_address_16.rds")
gc()
gc()
# 17
object_Vietnam_address_17 <- object_Vietnam_sample_group %>%filter(group == "group17") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_17,"object_Vietnam_address_17.rds")
gc()
gc()
# 18
object_Vietnam_address_18 <- object_Vietnam_sample_group %>%filter(group == "group18") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_18,"object_Vietnam_address_18.rds")
gc()
gc()
# 19
object_Vietnam_address_19 <- object_Vietnam_sample_group %>%filter(group == "group19") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_19,"object_Vietnam_address_19.rds")
gc()
gc()
# 20
object_Vietnam_address_20 <- object_Vietnam_sample_group %>%filter(group == "group20") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_20,"object_Vietnam_address_20.rds")
gc()
gc()
# 21
object_Vietnam_address_21 <- object_Vietnam_sample_group %>%filter(group == "group21") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_21,"object_Vietnam_address_21.rds")
gc()
gc()
# 22
object_Vietnam_address_22 <- object_Vietnam_sample_group %>%filter(group == "group22") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_22,"object_Vietnam_address_22.rds")
gc()
gc()
# 23
object_Vietnam_address_23 <- object_Vietnam_sample_group %>%filter(group == "group23") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_23,"object_Vietnam_address_23.rds")
gc()
gc()
# 24
object_Vietnam_address_24 <- object_Vietnam_sample_group %>%filter(group == "group24") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_24,"object_Vietnam_address_24.rds")
gc()
gc()
# 25
object_Vietnam_address_25 <- object_Vietnam_sample_group %>%filter(group == "group25") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_25,"object_Vietnam_address_25.rds")
gc()
gc()
# 26
object_Vietnam_address_26 <- object_Vietnam_sample_group %>%filter(group == "group26") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_26,"object_Vietnam_address_26.rds")
gc()
gc()
# 27
object_Vietnam_address_27 <- object_Vietnam_sample_group %>%filter(group == "group27") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
saveRDS(object_Vietnam_address_27,"object_Vietnam_address_27.rds")
gc()
gc()







































# # set concurrent computing plan
# # multisession: use CPUs as many as possible
# plan(multisession)
# # obtain address from shapefiles
# object_Vietnam_address <-
#   object_Vietnam_sample %>%
#   # obtain area information from the shapefile
#   dplyr::mutate(
#     # using furrr() package enables us to concurrent computing!!
#     # It raises up computation period dramatically!!
#     # In detail, refer to the following page.
#     # https://blog.atusy.net/2018/12/06/furrr/
#     # Previously, we used to use the following code with purrr::map2_dfr()
#     # area_info = purrr::map2_dfr(
#     area_info = furrr::future_map2(
#       .x = lon,
#       .y = lat,
#       ~
#         find_city(
#           sp_polygon = shp_Vietnam,
#           lon = .x,
#           lat = .y
#         )
#     )
#   ) %>%
#   tibble()
# object_Vietnam_address
# object_Vietnam_address$area_info
# 
# 
# saveRDS(object_Vietnam_address, "object_Vietnam_address.rds")
# rm(object_Vietnam_address)
# gc()
# gc()
# 
# object_Vietnam_address <- readRDS("object_Vietnam_address.rds")
# 
# 
# 
# # competed!!
# object_Vietnam <- 
#   bind_cols(
#     object_Vietnam_address$area_info, 
#     object_Vietnam_data
#   ) %>% 
#   select(-area_info, -true_false)
# # save the completed data
# saveRDS(object_Vietnam, "object_Vietnam.rds")
# 
# #
# #
# ##
# ### END ---
# ##
# #
