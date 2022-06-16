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
object_Vietnam_data <- sf::st_read("./object/Vietnam.geojsonl")
# Remove objects when not in use
# rm(object_Vietnam)
# read shapefiles by country
shp_Vietnam <- 
  sf::st_read(
    "./shapefiles/VNM_adm2.shp", 
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
    # for inspecting function movement
    print(res)
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
object_Vietnam_lat_lon <- 
  readRDS("object_Vietnam_lat_lon.rds") %>% 
  dplyr::mutate(
    id = c(1:nrow(.))) %>%
  dplyr::select(-true_false, -area) %>% 
  sf::st_drop_geometry() 

# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)
# obtain address from shapefiles
object_Vietnam_address <-
  object_Vietnam_lat_lon %>%
  # obtain area information from the shapefile
  dplyr::mutate(
    # using furrr() package enables us to concurrent computing!!
    # It raises up computation period dramatically!!
    # In detail, refer to the following page.
    # https://blog.atusy.net/2018/12/06/furrr/
    # Previously, we used to use the following code with purrr::map2_dfr()
    # area_info = purrr::map2_dfr(
    area_info = furrr::future_map2(
      .x = lon,
      .y = lat,
      ~
        find_city(
          sp_polygon = shp_Vietnam,
          lon = .x,
          lat = .y
        )
    )
  ) %>%
  tibble()
object_Vietnam_address
object_Vietnam_address$area_info


saveRDS(object_Vietnam_address, "object_Vietnam_address.rds")
rm(object_Vietnam_address)
gc()
gc()

object_Vietnam_address <- readRDS("object_Vietnam_address.rds")



# competed!!
object_Vietnam <- 
  bind_cols(
    object_Vietnam_address$area_info, 
    object_Vietnam_data
  ) %>% 
  select(-area_info, -true_false)
# save the completed data
saveRDS(object_Vietnam, "object_Vietnam.rds")

#
#
##
### END ---
##
#
