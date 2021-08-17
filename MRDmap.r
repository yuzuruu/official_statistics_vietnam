#############################################################
# Mekong delta map
# 12th. August 2021
# 14th. August 2021 revised

#### ---- load.objects ---- ####
# load libraries
library(countrycode)
library(GADMTools)
library(ggmap)
library(ggsn)
library(khroma)
library(maptools)
library(osmdata)
library(sp)
library(stringr)
library(tidyverse)
library(viridis)
#### ---- load.Vietnam.map ----
# Load country map with all provinces
vn_prov_map  <-  
  GADMTools::gadm_sf_loadCountries(
    "VNM", 
    level = 1, 
    basefile = "./"
    )
# Plot an administrative boundaries of Vietnam 
gadm_plot(vn_prov_map)
# name of provinces
listNames(vn_prov_map, level = 1)
#
##
### END of section --- ###
##
#

#### ---- Mekong.delta.map ----
# # Extract provinces in the Mekong River Delta from the list
# mrd  <- 
#   GADMTools::gadm_subset(
#     vn_prov_map, 
#     regions = c(
#       "An Giang",
#       "Bạc Liêu", 
#       "Bến Tre",
#       "Cần Thơ", 
#       "Cà Mau", 
#       "Đồng Tháp",
#       "Hậu Giang", 
#       "Kiên Giang", 
#       "Long An",
#       "Sóc Trăng", 
#       "Tiền Giang", 
#       "Trà Vinh",
#       "Vĩnh Long"
#       )
#     )
# # plot a map the Mekong delta region 
# GADMTools::gadm_plot(mrd) # missing three provinces
# # show the subset of province to make sure whether the filter worked
# mrd$sf$NAME_1
# 
# NOTE
# 1. The object named "mrd" composed of four list-type objects; 
# basedname, sf, level, and has BGND.
# To confirm the object's structure, please use str() function.
# We are able to find the listed objects above.
# 
# >str(mrd)
# 
# Even though we may lost the names, RStudio will 
# fill the name automatically.
# 
# 2. To extract content of the listed object, please use "$" string.
# This time, let us show the sf object.
# 
# >mrd$sf
# 
# 3. Executing the function, mrd$sf, we obtain the following results.
# > mrd$sf
# Simple feature collection with 13 features and 5 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 103.4588 ymin: 8.381355 xmax: 106.8225 ymax: 11.03222
# Geodetic CRS:  WGS 84
# First 10 features:
#    ISO  NAME_0      NAME_1      TYPE_1 ENGTYPE_1                       geometry
# 1  VNM Vietnam    An Giang        Tỉnh  Province MULTIPOLYGON (((105.3745 10...
# 12 VNM Vietnam   Bạc Liêu        Tỉnh  Province MULTIPOLYGON (((105.4164 9....
# 56 VNM Vietnam     Bến Tre        Tỉnh  Province MULTIPOLYGON (((106.6355 9....
# 4  VNM Vietnam     Cần Thơ Thành phố       City MULTIPOLYGON (((105.5471 9....
# 5  VNM Vietnam     Cà Mau        Tỉnh  Province MULTIPOLYGON (((104.8791 8....
# 10 VNM Vietnam Đồng Tháp        Tỉnh  Province MULTIPOLYGON (((105.8773 10...
# 17 VNM Vietnam   Hậu Giang        Tỉnh  Province MULTIPOLYGON (((105.757 9.6...
# 27 VNM Vietnam Kiên Giang        Tỉnh  Province MULTIPOLYGON (((103.4684 9....
# 33 VNM Vietnam     Long An        Tỉnh  Province MULTIPOLYGON (((106.0896 10...
# 47 VNM Vietnam Sóc Trăng        Tỉnh  Province MULTIPOLYGON (((105.7068 9....
# 
# Because of specification of R, the first 10 names of the province (NAME_1) 
# are displayed. In this case, "Tiền Giang"  "Trà Vinh"   "Vĩnh Long" are 
# hidden. They, however, are loaded precisely. To show the rest of the province,
# please use the following function.
# 
# > mrd$sf$NAME_1
# 
# # Your R codes appears to work well!!
#
##
### END of section --- ###
##
#
### ---- population.map ----
# Preparation
# 1. Download sf data from GADM ()
# To make full use of GADM data, it is necessary to download the sf data
# from the GADM website.
# 
# 2. For filling color, we use data on population by province.
# We downloaded the population data from GSO.
# After downloading, revise the file a little as attached.
# To make sure whether we missed the data of 3 provinces,
# we filled the color. If the data would be missed, we would
# not be able to fill them.
# 
# 3. Put the two data into the same directory of the r. file. 
# 
# 

# read sf data
vn_prov_map <- 
  base::readRDS("gadm36_VNM_1_sf.rds")
# read population data by MSExcel
vn_prov_population <- 
  readxl::read_excel(
    path = "population_vnm.xlsx",
    sheet = "population",
    range = "A4:B74",
    col_names = TRUE
    )
#
# read target provinces' names
# To avoid problems of regional font, we use English names
# of the target province. However, we will also challenge
# using Vietnamese names.
# Note
# Provinces' names by GSO and VANAME_1 (provinces' names in the sf file)
# do not often match. When the names do not match, we cannot merge
# the two data. Before / after reading the data, please check differences
# of the name carefully.
# Example:
# GSO: Kien  Giang (two spaces between "Kien" and "Giang")
# GADM: Kien Giang (one space between "Kien" and "Giang")
mrd_province <- 
  c(
    "An Giang",
    "Bac Lieu",
    "Ben Tre",
    "Can Tho",
    "Ca Mau",
    "Dong Thap",
    "Hau Giang",
    "Kien Giang",
    "Long An",
    "Soc Trang",
    "Tien Giang",
    "Tra Vinh",
    "Vinh Long"
  )
# pick up the target region
# %in% is a logical operator to set union set.
# The case below choose observation of VARNAME_1 including any of an element of mrd_province.
# In detail of the useful operator, plese refer to the following page.
# https://www.marsja.se/how-to-use-in-in-r/
mrd <- 
  vn_prov_map %>% 
  dplyr::filter(
    VARNAME_1 %in% mrd_province
  )
# check whether the subset of vn_prov_map (mrd) can be use correctly.
mrd_map <- 
  mrd %>% 
  ggplot2::ggplot() +
  geom_sf() +
  theme_classic()
# obtain population data of the target province
vn_prov_population_mrd <- 
  vn_prov_population %>% 
  dplyr::filter(
    province %in% mrd_province
  )
# obtain centroid (geographical center of province) of the target provinces
# We use the data to place provinces' name on the map later.
mrd_centroid <- 
  mrd %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) %>%
  # remove geometry and centroid for convenient use
  tidyr::as_tibble() %>% 
  # select necessary variables
  dplyr::select(
    VARNAME_1,        
    center_x,
    center_y
  )
# merge the population and centroid data
# into the sf data
mrd_population <- 
  mrd %>% 
  # merge sf data and population data
  dplyr::left_join(
    vn_prov_population_mrd,
    by = c("VARNAME_1" = "province")
  ) %>% 
  # merge the joined data and centroid data
  dplyr::left_join(
    mrd_centroid,
    by = "VARNAME_1"
  )
# draw a MDR map
mrd_map <- 
  mrd_population %>% 
  ggplot2::ggplot(
    aes(
      fill = population
      )
    ) +
  # geom_sf() is a popular function to draw a map
  # In detail of the geom_sf(), please refer to the following page.
  # https://ggplot2.tidyverse.org/reference/ggsf.html
  # It is loaded together when we load the sf() package.
  geom_sf(
    # set alpha channel 
    alpha = 0.7
    ) +
  # Fill colors in accordance with khroma color scheme.
  # The scheme meets so-called color universal design,
  # a policy friendly to color blindness.
  # In detail of the khroma color scheme, please refer to the following page.
  # https://packages.tesselle.org/khroma/
  scale_fill_smoothrainbow(
    discrete = FALSE
    ) +
  # add English names of the target provinces
  geom_text(
    aes(
      x = center_x,
      y = center_y,
      label = VARNAME_1,
      # adjust font size when necessary
      size = 5
    ),
    show.legend = FALSE,
    family = "Times"
  ) +
  # add Vietnamese names of the target provinces
  # To avoid overlap, set vertical justification
  # When we do not need the name in Vietnamese,
  # comment out this part below.
  geom_text(
    aes(
      x = center_x,
      y = center_y,
      # refer to the Vietnamese name
      label = NAME_1,
      # adjust font size when necessary
      size = 5,
      vjust = 2
    ),
    show.legend = FALSE,
    # set font family
    # Setting the family enables us to use the Vietnamese names and fonts.
    family = "Arial"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Population (Unit: 1,000 pax)"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.position = c(0.25, 0.25),
    legend.direction = "vertical",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm")
  ) 
# save the map
# Adjust the width and height when necessary
ggsave(
  "mrd_map.pdf",
  plot = mrd_map,
  # set plot area size in mm.
  width = 300,
  height = 300,
  units = "mm",
  # set device to draw the pdf file
  # When we draw pdf-formatted file with variety of language,
  # it is necessary to use the cairo_pdf.
  # When we draw other-format images such as jpg, 
  # we need to set other devices.
  device = cairo_pdf # important!!
)

#
##
### END of section --- ###
##
#


#### ---- osm.map ----
# check features and tags
# We often use "concealed" tags. In detail, refer to the following pages.
# Open street map features
# https://wiki.openstreetmap.org/wiki/Map_features
# 
# available_features()
# available_tags("highway")
# available_tags("landuse")
# available_tags("waterway")
# 
# obtain boundary box (bb)
# Normally, we obtain the bb from name(s) of specific place(s) using getbb() function.
# When we obtain information the Mekong Delta region, such a way was not available.
# Instead, using sf object, we obtained the boundary box with sf::st_bbbox() and then
# provide the bb into opq() function.
mrd_bb <- 
  mrd %>% sf::st_bbox()


# administrative boundaries
adm <- readRDS("gadm36_VNM_1_sf.rds")
khm <- readRDS("gadm36_KHM_0_sf.rds")

vnm_ex_mrd <- 
  adm %>% 
  dplyr::filter(
    !(VARNAME_1 %in% mrd_province)
    ) %>% 
  sf::st_union() 

mrd_union <- 
  adm %>% 
  dplyr::filter(
    VARNAME_1 %in% mrd_province
  ) %>% 
  sf::st_union()
# 
# obtain features' data using osmdata()
# street
# "Street" refers to small roads excluding motorway and major road.
streets <- 
  mrd_bb %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "footway", 
      "residential", 
      "service", 
      "track",
      "residential", 
      "living_street",
      "service",
      "unclassified"
      )
    ) %>%
  osmdata::osmdata_sf()
streets
# road
road <- 
  mrd_bb %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway", 
      "motorway_junction",
      "motorway_link",
      "primary", 
      "primary_link",
      "secondary", 
      "secondary_link",
      "tertiary",
      "tertiary_link",
      "trunk",
      "trunk_link"
    )
  ) %>%
  osmdata::osmdata_sf()
road
# river
# NOTE
# The feature, "river", only returns center of rivers. To obtain
# width / shape of the river use "natural" instead as below.
river <- 
  mrd_bb %>%
  opq()%>%
  add_osm_feature(
    key = "waterway", 
    value = c(
      "river"
    )
  ) %>%
  osmdata_sf()
# riverbank (= riverbed)
# In detail of difference between river and riberbank, refer to the following page.
# https://wiki.openstreetmap.org/wiki/Rivers
riverbank <- 
  mrd_bb %>%
  opq()%>%
  add_osm_feature(
    key = "natural", 
    value = c(
      "water"
    )
  ) %>%
  osmdata_sf()
# canal
canal <- 
  mrd_bb %>%
  opq()%>%
  add_osm_feature(
    key = "waterway", 
    value = c(
      "canal"
    )
  ) %>%
  osmdata_sf()
#
# Draw a multi-layered map
mdr_multilayer_map <- 
  ggplot() +
  geom_sf() +
  # street
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = "grey50",
    size = 0.2,
    alpha = 1.0
  ) +
  # road
  geom_sf(
    data = road$osm_lines,
    inherit.aes = FALSE,
    color = "orange",
    size = 0.2,
    alpha = 1.0
  ) +
  # river
  geom_sf(
    data = river$osm_lines,
    inherit.aes = FALSE,
    color = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # riberbed
  geom_sf(
    data = riverbank$osm_multipolygons,
    inherit.aes = FALSE,
    color = "steelblue",
    fill = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # canal
  geom_sf(
    data = canal$osm_lines,
    inherit.aes = FALSE,
    color = "steelblue",
    size = 0.2,
    alpha = 1.0
  ) +
  # province-level administrative boundaries
  geom_sf(
    data = adm,
    inherit.aes = FALSE,
    fill = NA,
    color = "black",
    size = 0.3,
    alpha = 1.0
  ) +
  # administrative boudaries of the Mekong Delta region
  geom_sf(
    data = mrd_union,
    inherit.aes = FALSE,
    color = "black",
    fill = NA,
    size = 0.8,
    alpha = 1.0
  ) +
  # paint Cambodia and northern part of Vietnam in white
  geom_sf(
    data = vnm_ex_mrd,
    inherit.aes = FALSE,
    color = "black",
    fill = "white",
    size = 0.8,
    alpha = 1.0
  ) +
  # paint Cambodia and northern part of Vietnam in white
  geom_sf(
    data = khm,
    inherit.aes = FALSE,
    color = "black",
    fill = "white",
    size = 0.8,
    alpha = 1.0
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "\U00a9 OpenStreetMap contributors"
  ) +
  # fix boundary box
  coord_sf(xlim = c(104.4, 107),
           ylim = c(8.5, 11.1),
           expand = FALSE) +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = NA)
    )
mdr_multilayer_map

# 
# save the results
ggsave(
  "mdr_multilayer_map.pdf", 
  plot = mdr_multilayer_map
  )
# 
# add provinces' name, scalebar, and north arrow to the previous map 
mdr_multilayer_map_02 <- 
  mdr_multilayer_map +
  # provinces' name
  annotate(
    geom = "label",
    x = mrd_centroid$center_x,
    y = mrd_centroid$center_y,
    label = mrd_centroid$VARNAME_1,
    size = 3,
    family = "Times",
    fill = "white"
  ) +
  # scalebar
  ggsn::scalebar(
    x.min = 105.4,
    x.max = 106.4,
    y.min =8.6,
    y.max = 8.7,
    dist = 50, 
    dist_unit = "km",
    st.dist = 0.3, 
    st.size = 3, 
    height= 0.3, 
    transform = TRUE
    ) +
  # north arrow
  ggsn::north(
    x.min = 106.14,
    x.max = 107.14,
    y.min =8.53,
    y.max = 8.73,
    symbol = 8,
    scale = 1
  )

mdr_multilayer_map_02

ggsave(
  "mdr_multilayer_map_02.pdf", 
  plot = mdr_multilayer_map_02
)

#
##
### END of section --- ###
##
#


