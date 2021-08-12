#############################################################
# Mekong delta map
# 12th. August 2021
#

#### ---- load.library ---- ####
# load libraries
library(GADMTools)
library(tidyverse)
library(stringi)
library(maptools)
library(countrycode)
library(sp)

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
# Extract provinces in the Mekong River Delta from the list
mrd  <- 
  GADMTools::gadm_subset(
    vn_prov_map, 
    regions = c(
      "An Giang",
      "Bạc Liêu", 
      "Bến Tre",
      "Cần Thơ", 
      "Cà Mau", 
      "Đồng Tháp",
      "Hậu Giang", 
      "Kiên Giang", 
      "Long An",
      "Sóc Trăng", 
      "Tiền Giang", 
      "Trà Vinh",
      "Vĩnh Long"
      )
    )
# plot a map the Mekong delta region 
GADMTools::gadm_plot(mrd) # missing three provinces
# show the subset of province to make sure whether the filter worked
mrd$sf$NAME_1

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



