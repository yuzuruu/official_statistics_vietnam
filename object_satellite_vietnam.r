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
library(ggsn)
library(furrr)
library(rsample)
# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)

# 
# # ---- read.data ----
# # read object data
# # The .geojson data is huge. It takes long time.
# # object_Vietnam_data <-
# sf::st_read("./object/Vietnam.geojsonl")
# # Remove objects when not in use
# # read the shapefiles by country
# shp_Vietnam <-
#   sf::st_read(
#     "./shapefiles/VNM_adm2.shp",
#     options = "ENCODING=UTF-8",
#     stringsAsFactors=FALSE
#   ) %>%
#   dplyr::mutate_if(
#     is.character,
#     enc2utf8
#   )
# # ---- read.function ----
# # a function to find address from lat / lon while using the shapefiles
# # We thank following links.
# # https://qiita.com/nozma/items/808bce2f496eabd50ff1
# # https://qiita.com/uri/items/69b2c05f7b3a21d3aad3
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
  if(identical(which.row, integer(0)) == TRUE) {
    # message("Assigned coordinates are not included at all.")
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
# # ---- make.area.data ----
# #  Calculate area of objects from the .geojson file
# # # WARNING
# # # This process needs long computation periods.
# object_Vietnam_lat_lon <-
#   # provide data
#   object_Vietnam_data %>%
#   # evaluate the geometries whether they are validated
#   # If not, functions below will not work.
#   # In detail, refer to the following page.
#   # https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using
#   dplyr::mutate(
#     true_false = sf::st_is_valid(.)
#   ) %>%
#   # select vali data
#   dplyr::filter(true_false == "TRUE") %>%
#   # add longitude and latitude
#   dplyr::mutate(
#     area = sf::st_area(.),
#     lon = st_coordinates(sf::st_centroid(.))[,1],
#     lat = st_coordinates(sf::st_centroid(.))[,2]
#   )
# saveRDS(object_Vietnam_lat_lon, "object_Vietnam_lat_lon.rds")
# rm(object_Vietnam_lat_lon)
# gc()
# gc()
# 
# # ---- obtain.address.from.shapefiles ----
# # read the lon-lat file
# # object_Vietnam_sample <-
# #   readRDS("object_Vietnam_lat_lon.rds") %>%
# #   # add id for random sampling
# #   dplyr::mutate(
# #     id = c(1:nrow(.))) %>%
# #   dplyr::select(-true_false, -area) %>%
# #   # for faster computation, remove the geometry
# #   sf::st_drop_geometry()
# #
# # Separate the target file into a certain parts randomly
# # set.seed(123)
# # obtain group id
# # NOTE
# # We need to separate the data without surplus
# # This time, our data can be divided by an appropriate number (27).
# idx <- sample(1:nrow(object_Vietnam_sample)/27)
# cv <-
#   split(
#     idx,
#     ceiling(
#       seq_along(idx) / floor(length(idx) / 27
#       )
#     )
#   ) %>%
#   bind_rows() %>%
#   as_tibble() %>%
#   data.table::setnames(
#     c(
#       "group01","group02","group03","group04","group05","group06","group07","group08","group09","group10",
#       "group11","group12","group13","group14","group15","group16","group17","group18","group19","group20",
#       "group21","group22","group23","group24","group25","group26","group27"
# 
#     )
#   ) %>%
#   tidyr::pivot_longer(
#     cols = everything(),
#     names_to = "group",
#     values_to = "id"
#   )
# # combine the original data and randomly-allocated group
# object_Vietnam_sample_group <-
#   object_Vietnam_sample %>%
#   left_join(
#     cv,
#     by = "id"
#   )
# #
# # setting for furrr package
# plan(multisession)
# # obtain provinces' name by point
# # 01
# object_Vietnam_address_01 <- object_Vietnam_sample_group %>%filter(group == "group01") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_01,"object_Vietnam_address_01.rds")
# gc()
# gc()
# # 02
# object_Vietnam_address_02 <- object_Vietnam_sample_group %>%filter(group == "group02") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_02,"object_Vietnam_address_02.rds")
# gc()
# gc()
# # 03
# object_Vietnam_address_03 <- object_Vietnam_sample_group %>%filter(group == "group03") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_03,"object_Vietnam_address_03.rds")
# gc()
# gc()
# # 04
# object_Vietnam_address_04 <- object_Vietnam_sample_group %>%filter(group == "group04") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_04,"object_Vietnam_address_04.rds")
# gc()
# gc()
# # 05
# object_Vietnam_address_05 <- object_Vietnam_sample_group %>%filter(group == "group05") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_05,"object_Vietnam_address_05.rds")
# gc()
# gc()
# # 06
# object_Vietnam_address_06 <- object_Vietnam_sample_group %>%filter(group == "group06") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_06,"object_Vietnam_address_06.rds")
# gc()
# gc()
# # 07
# object_Vietnam_address_07 <- object_Vietnam_sample_group %>%filter(group == "group07") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_07,"object_Vietnam_address_07.rds")
# gc()
# gc()
# # 08
# object_Vietnam_address_08 <- object_Vietnam_sample_group %>%filter(group == "group08") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_08,"object_Vietnam_address_08.rds")
# gc()
# gc()
# # 09
# object_Vietnam_address_09 <- object_Vietnam_sample_group %>%filter(group == "group09") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_09,"object_Vietnam_address_09.rds")
# gc()
# gc()
# # 10
# object_Vietnam_address_10 <- object_Vietnam_sample_group %>%filter(group == "group10") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_10,"object_Vietnam_address_10.rds")
# gc()
# gc()
# # 11
# object_Vietnam_address_11 <- object_Vietnam_sample_group %>%filter(group == "group11") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_11,"object_Vietnam_address_11.rds")
# gc()
# gc()
# # 12
# object_Vietnam_address_12 <- object_Vietnam_sample_group %>%filter(group == "group12") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_12,"object_Vietnam_address_12.rds")
# gc()
# gc()
# # 13
# object_Vietnam_address_13 <- object_Vietnam_sample_group %>%filter(group == "group13") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_13,"object_Vietnam_address_13.rds")
# gc()
# gc()
# # 14
# object_Vietnam_address_14 <- object_Vietnam_sample_group %>%filter(group == "group14") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_14,"object_Vietnam_address_14.rds")
# gc()
# gc()
# # 15
# object_Vietnam_address_15 <- object_Vietnam_sample_group %>%filter(group == "group15") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_15,"object_Vietnam_address_15.rds")
# gc()
# gc()
# # 16
# object_Vietnam_address_16 <- object_Vietnam_sample_group %>%filter(group == "group16") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_16,"object_Vietnam_address_16.rds")
# gc()
# gc()
# # 17
# object_Vietnam_address_17 <- object_Vietnam_sample_group %>%filter(group == "group17") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_17,"object_Vietnam_address_17.rds")
# gc()
# gc()
# # 18
# object_Vietnam_address_18 <- object_Vietnam_sample_group %>%filter(group == "group18") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_18,"object_Vietnam_address_18.rds")
# gc()
# gc()
# 19
# object_Vietnam_address_19 <- object_Vietnam_sample_group %>%filter(group == "group19") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_19,"object_Vietnam_address_19.rds")
# gc()
# gc()
# # 20
# object_Vietnam_address_20 <- object_Vietnam_sample_group %>%filter(group == "group20") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_20,"object_Vietnam_address_20.rds")
# gc()
# gc()
# # 21
# object_Vietnam_address_21 <- object_Vietnam_sample_group %>%filter(group == "group21") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_21,"object_Vietnam_address_21.rds")
# gc()
# gc()
# # 22
# object_Vietnam_address_22 <- object_Vietnam_sample_group %>%filter(group == "group22") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_22,"object_Vietnam_address_22.rds")
# gc()
# gc()
# # 23
# object_Vietnam_address_23 <- object_Vietnam_sample_group %>%filter(group == "group23") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_23,"object_Vietnam_address_23.rds")
# gc()
# gc()
# # 24
# object_Vietnam_address_24 <- object_Vietnam_sample_group %>%filter(group == "group24") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_24,"object_Vietnam_address_24.rds")
# gc()
# gc()
# # 25
# object_Vietnam_address_25 <- object_Vietnam_sample_group %>%filter(group == "group25") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_25,"object_Vietnam_address_25.rds")
# gc()
# gc()
# # 26
# object_Vietnam_address_26 <- object_Vietnam_sample_group %>%filter(group == "group26") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_26,"object_Vietnam_address_26.rds")
# gc()
# gc()
# # 27
# object_Vietnam_address_27 <- object_Vietnam_sample_group %>%filter(group == "group27") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Vietnam, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Vietnam_address_27,"object_Vietnam_address_27.rds")
# gc()
# gc()
# 
# # ---- make.address.geometry.file ----
# # merge the 27 of address files altogether
# object_Vietnam_address <-
#   # obtain path list of the separated address file
#   list.files(path = "./address", pattern = "*.rds") %>%
#   # add strings to make complete paths
#   paste0("./address/",.) %>%
#   # read the target files listed above
#   purrr::map_df(
#     .,
#     readRDS
#   ) %>%
#   # rows containing NA (lgl[1]) list
#   # if the variable named "area_info" contains NA, it will not be "tibble" but "lgl".
#   # Using the characteristics, we remove the list sorely with NA.
#   dplyr::filter(
#     map_lgl(
#       area_info,
#       is_tibble
#     )
#   ) %>%
#   # bind the lists containing address
#   mutate(
#     areainfo = dplyr::bind_rows(.$area_info)
#   ) %>%
#   # pick up necessary info
#   mutate(
#     province_code = areainfo$province_code,
#     province_name = areainfo$province_name,
#     district_code = areainfo$district_code,
#     district_name = areainfo$district_name
#   ) %>%
#   # select necessary variables
#   select(lon, lat, id, group, province_code, province_name, district_code, district_name)
# # save the results
# saveRDS(object_Vietnam_address, "object_Vietnam_address.rds")
# # save the results as a .csv file
# # NOTE
# # The csv file is huge. No apps can open that.
# readr::write_excel_csv(
#   object_Vietnam_address,
#   "object_Vietnam_address.csv"
# )
# 
# # ---- draw.map ----
# # Final session
# # read the lat-lon data with geometry
# object_Vietnam_lat_lon <-
#   readRDS("object_Vietnam_lat_lon.rds") %>%
#   dplyr::mutate(
#     id = c(1:nrow(.))) %>%
#   dplyr::select(-true_false, -lat, -lon)
# # read the address data
# object_Vietnam_address <-
#   readRDS("object_Vietnam_address.rds")
# # combine necessary information altogerther
# # NOTE
# # information:
# # lat
# # lon
# # area of the building footprint
# # province and district
# # geometry (polygon data)
# object_Vietnam <-
#   object_Vietnam_lat_lon %>%
#   dplyr::inner_join(
#     object_Vietnam_address,
#     by = "id"
#     )
# # save the data
# # This is what we would like to obtain.
# # Once we generate this data, we need not to compile previous
# # code and make this file.
# # If you need to compile that, please release the comment out
# # and implement.
# saveRDS(object_Vietnam, "object_Vietnam.rds")
# #
# # read the address data
# object_Vietnam <- readRDS("object_Vietnam.rds")
# # pick up the address data belonging to the Mekong Delta region
object_Mekong <-
  readRDS("object_Vietnam.rds") %>%
  dplyr::filter(
    province_name %in% c(
      "Đồng Tháp",
      "An Giang",
      "Long An",
      "Bạc Liêu",
      "Bến Tre",
      "Cà Mau",
      "Cần Thơ",
      "Hậu Giang",
      "Kiên Giang",
      "Sóc Trăng",
      "Tiền Giang",
      "Trà Vinh",
      "Vĩnh Long"
      ))
# read a shapefile to draw administrative boudaries
# map_Mekong <-
#   sf::read_sf("./shapefiles/VNM_adm1.shp") %>%
#   dplyr::filter(
#     NAME_1 %in% c(
#       "Đồng Tháp",
#       "An Giang",
#       "Long An",
#       "Bạc Liêu",
#       "Bến Tre",
#       "Cà Mau",
#       "Cần Thơ",
#       "Hậu Giang",
#       "Kiên Giang",
#       "Sóc Trăng",
#       "Tiền Giang",
#       "Trà Vinh",
#       "Vĩnh Long"
#     ))
# #
# # Draw maps
# object_Mekong_map <-
#   object_Mekong %>%
#   ggplot()+
#   geom_sf(data = map_Mekong, fill = NA, inherit.aes = FALSE, size = 0.1) +
#   geom_sf(size = 0.01) +
#   labs(
#     title = "Map of building footprint in the Mekong Delta region",
#     subtitle = "We obtained the data from the Microsoft Inc. \n (https://github.com/microsoft/GlobalMLBuildingFootprints#will-there-be-more-data-coming-for-other-geographies)",
#     x = "Longitude",
#     y = "Latitude"
#     ) +
#   theme_classic() +
#   theme(
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 30)
#   ) +
#   scalebar(
#     data = object_Mekong,
#     dist = 50,
#     dist_unit = "km",
#     transform = TRUE,
#     location = "bottomleft"
#     ) +
#   north(
#     data = object_Mekong,
#     symbol = 16,
#     location = "bottomright"
# 
#   )
# #
# # save the map with .pdf format
# # NOTE
# # Size of the map is huge. It takes long time to display.
# # When you would like to look at a building, please enlarge.
# # For size of the buildings, the map is huge. Enlarging the
# # map, you will be able to find the shape of the building.
# # At a sight, the buildings looks like mass of dots.
# ggsave(
#   "object_Mekong_map.pdf",
#   plot = object_Mekong_map,
#   width = 500,
#   height = 500,
#   units = "mm",
#   limitsize = FALSE
#   )


object_Mekong_summary <- 
  object_Mekong %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(province_name) %>% 
  dplyr::summarise(
    N = n(),
    Min. = min(area),
    Mean = mean(area),
    Median = median(area),
    Max. = max(area),
    SD = sd(area)
  )

object_Mekong_top100 <- 
  object_Mekong %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(province_name) %>% 
  dplyr::arrange(desc(area)) %>% 
  slice(1:100)
readr::write_excel_csv(
  object_Mekong_top100,
  "object_Mekong_top100.csv"
)

#
##
### --- END --- ###
##
#

# ---- analyse.specific.area ----
# NOTE
# Once we finished obtaining adresses, we need not
# to run the following code.
# comment out when not in use.
# The Mekong Delta region
# make shapefiles of the target area
shp_Mekong <-
  sf::st_read(
    "./shapefiles/VNM_adm3.shp",
    options = "ENCODING=UTF-8",
    stringsAsFactors=FALSE
  ) %>%
  dplyr::mutate_if(
    is.character,
    enc2utf8
  ) %>% 
  dplyr::filter(
      NAME_1 %in% c(
        "Đồng Tháp",
        "An Giang",
        "Long An",
        "Bạc Liêu",
        "Bến Tre",
        "Cà Mau",
        "Cần Thơ",
        "Hậu Giang",
        "Kiên Giang",
        "Sóc Trăng",
        "Tiền Giang",
        "Trà Vinh",
        "Vĩnh Long"
      )
      )

# read ****** data
object_Vietnam_sample_group_mekong <-
  # read data
  readRDS(
    "object_Vietnam_sample_group.rds"
  ) %>%
  # fix target area roughly
  # We can obtain the condition using Google Maps
  dplyr::filter(
    lat < 11.032
    ) 
# This group is just for "for loop". The number includes
# no meaning.
group_2803 <-
  levels(
    factor(
      object_Vietnam_sample_group_mekong$group
      )
  )
# obtain address by group and object induvidually
object_Vietnam_sample_group_address_mekong <-
  for(i in 1:length(group_2803)){
    target <- filter(
      object_Vietnam_sample_group_mekong,
      group == group_2803[i]
    )
    target_address <-
      target %>%
      dplyr::mutate(
        area_info = furrr::future_map2_dfr(
          .x = lon,
          .y = lat,
          ~ try(
            find_city(
              sp_polygon = shp_Mekong,
              lon = .x,
              lat = .y
            )
          )
        )
      ) %>%
      tibble()
    # save the computation results
    write_excel_csv(
      # fix target column
      bind_cols(
        id = target_address$id,
        province_code = target_address$area_info$province_code,
        province_name = target_address$area_info$province_name,
        district_code = target_address$area_info$district_code,
        district_name = target_address$area_info$district_name,
        town_code = target_address$area_info$town_code,
        town_name = target_address$area_info$town_name
      ) %>%
        # remove rows containing NA
        na.omit(),
      file = paste0("target_address_mekong/",target_address$group[1], ".csv")
    )
    # for larger data, enable the gc() function
    # gc()
    # gc()
  }
# make a list of generated csv files
# target_file_list <- 
#   dir(
#     path = "target_address_camau", 
#     pattern = "*.csv"
#   )
# combine altoghether
# Reference:
# https://qiita.com/Ringa_hyj/items/434e3a6794bb7ed8ee14
# target_address_camau_combined <- 
#   vroom::vroom(
#     paste0(
#       "target_address_camau/", 
#       target_file_list, 
#       sep = ""
#     )
#   ) %>% 
#   dplyr::filter(province_name == "Cà Mau")
# NOTE
# The code above works slowly. It is just for education.
# DO NOT USE.
# target_address_camau_combined <- 
#   target_file_list %>%
#   purrr::map(
#     ~
#       read_csv(
#         paste0(
#           "target_address_camau/", 
#           target_file_list, 
#           sep = ""
#         )
#       )
#   ) %>%
#   purrr::reduce(full_join, by = c("id", "province_code", "province_name", "district_code", "district_name", "town_code", "town_name"))
# save the combined csv files for safe use
# readr::write_excel_csv(
#   target_address_camau_combined,
#   "target_address_camau_combined.csv"
#   )
# join in the address file and geometry
# object_Vietnam_camau <-
#   readRDS("object_Vietnam_lat_lon.rds") %>%
#   dplyr::mutate(
#       id = c(1:nrow(.))
#       ) %>% 
#   dplyr::inner_join(
#     target_address_camau_combined,
#     by = "id"
#   ) %>% 
#   dplyr::select(
#     id, 
#     area, 
#     lon, 
#     lat,
#     province_code,
#     province_name,
#     district_code,
#     district_name,
#     town_code,
#     town_name,
#     geometry
#     )
# save the file
# Completed!!
# saveRDS(
#   object_Vietnam_mekong,
#   "object_Vietnam_mekong.rds"
# )
# 
# hoge <- 
#   sf::read_sf("shapefile_footprint_camau/object_Vietnam_camau.shp") 
# hogehoge <- 
#   ggplot(
#     hoge[c(1:347129),]
#   )+
#   geom_sf(aes(fill = area),lwd = 0) +
#   scale_fill_iridescent() +
#   theme_void() 
# ggsave("hoge.pdf", plot = hogehoge, width = 5000, height = 5000, units = "mm", limitsize = FALSE)
# 
