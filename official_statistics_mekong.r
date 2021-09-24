##############################################################################
# Socioeconomic data of the Mekong delta region by district
# 6th. September
# by Yuzuru Utsunomiya

##### ---- load.library ---- 
library(janitor)
library(khroma)
library(patchwork)
library(tidyverse)
library(sp)
library(khroma)

##### ---- read.data ----
# Read NLMM (Agriculture, Forestry, and Fishery) data
# 
# References
# 1. Reading files with multibyte string / character
# https://stackoverflow.com/questions/46503390/read-excel-correctly-imports-file-but-invalid-multibyte-string-error-when-try
# 2. Delete columns filled with NA all
# https://stackoverflow.com/questions/15968494/how-to-delete-columns-that-contain-only-nas
# 3. Replace columns' name without making an intermediate valiable
# https://keachmurakami.github.io/page/2016/04/01/%E4%B8%AD%E9%96%93%E7%94%9F%E6%88%90%E3%81%AA%E3%81%97%E3%81%A7%E3%82%AB%E3%83%A9%E3%83%A0%E5%90%8D%E3%82%92%E5%A4%89%E6%9B%B4%E3%81%99%E3%82%8B%EF%BC%95%E3%81%A4%E3%81%AE%E6%96%B9%E6%B3%95-r/
# 4. Normal expression in dplyr::filter() function
# https://stackoverflow.com/questions/22850026/filter-rows-which-contain-a-certain-string
# 5. Regular expression of R
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
# 6. A new function tidyr::pivot_longer(), compatible with tidyr:;gather()
# https://qiita.com/yanami/items/3775df6c579fd0a2d60c
#
# As for the other data sets (population, economy, industry, and others),
# we need to add some codes. At the moment, we focus on the industries
# and make this data set first.
# 
# make a list of target data
path_list <-
  base::list.files(path = "mekong_trial",
                   pattern ="*.xls",
                   recursive = TRUE
  ) %>%
  stringr::str_subset("Agriculture") %>%
  base::paste("./mekong_trial/", .,
              sep = ""
  ) %>%
  dplyr::tibble(path = .) 

# # read and make data set
# WARNING
# Before reading the MSExcel files, check format
# carefully to meet functions' demands below.
nlnn_mekong <-
  path_list %>% 
  # add a column of data
  dplyr::mutate(
    rawdata = purrr::map(path_list$path,
                         ~
                           readxl::read_excel(
                             path = .,
                             col_names = FALSE,
                             # avoid automatic recognition of data type
                             # We will transform the data time after
                             # making tidy data. Until then, we do not
                             # need the recognition. We will deal with 
                             # the data as text (as.character).
                             col_types = c("text"),
                             # specify target sheets position
                             # Some sheets are named and others not.
                             # To avoid confusion, we fix the position
                             # using ordering number.
                             sheet = 1
                             )
                         )
    ) %>% 
  dplyr::mutate(
    province = dplyr::case_when(
      stringr::str_detect(.$path, "BAC LIEU") ~ "Bac Lieu",
      stringr::str_detect(.$path, "BEN TRE") ~ "Ben Tre",
      stringr::str_detect(.$path, "CAMAU") ~ "Ca Mau",
      stringr::str_detect(.$path, "KIEN GIANG") ~ "Kien Giang",
      stringr::str_detect(.$path, "SOC TRANG") ~ "Soc Trang",
      stringr::str_detect(.$path, "TRA VINH") ~ "Tra Vinh",
      TRUE ~ "NA"
    )
  ) %>% 
  # add trait of each data
  dplyr::mutate(
    trait = purrr::map(
      .$rawdata,
      ~
        # obtain English title from the original table
        .[2,1]
    ) ,
    trait = as.character((unlist(trait)))
  ) %>% 
  # add trait of each data (Vietnamese)
  dplyr::mutate(
    trait_vietnamese = purrr::map(
      .$rawdata,
      ~
        # obtain English title from the original table
        .[1,1]
    ),
    trait_vietnamese = as.character(unlist(trait_vietnamese))
  ) %>% 
  # replace / unite name of trait (Vietnamese)
  dplyr::mutate(
    # Let us add conditions below accordingly.
    trait_vietnamese = dplyr::case_when(　　 
      # production of shrimp aquaculture
      stringr::str_detect(.$trait_vietnamese, "Sản lượng tôm nuôi") ~ "Sản lượng tôm nuôi", 
      # annual production of paddy rice
      # Sản lượng lúa cả năm phân theo huyện
      stringr::str_detect(.$trait_vietnamese, "Sản lượng lúa cả năm phân theo huyện") ~ "Sản lượng lúa cả năm phân theo huyện", 
      # winter spring paddy: Sản lượng lúa Đông xuân phân theo huyện
      # NOTE: Some sorts of paddy rice cultivation exist:
      #   Sản lượng lúa đông xuân phân theo huyện. Thị (BL)
      #   Sản lượng lúa đông xuân phân theo huyện, thành phố (CM)
      #   Sản lượng lúa Đông Xuân (KG)
      #   Sản lượng lúa Đông xuân phân theo huyện, thị (ST)
      #   Sản lượng lúa Đông xuân (TV)
      stringr::str_detect(.$trait_vietnamese, "Sản lượng lúa Đông xuân|Sản lượng lúa Đông Xuân|Sản lượng lúa đông xuân") ~ "Sản lượng lúa Đông xuân phân theo huyện", 
      # summer autumn paddy: Sản lượng lúa Hè thu phân theo huyện
      # NOTE: Some sorts of paddy rice cultivation exist:
      #   Sản lượng lúa hè thu phân theo huyện thị (BL)
      #   (BT)
      #   Sản lượng lúa hè thu phân theo huyện, thành phố (tấn) (CM) 
      #   Sản lượng lúa Hè thu (KG)
      #   Sản lượng lúa Hè thu phân theo huyện, thị (ST) 
      #   Sản lượng lúa Hè thu (TV) 
      stringr::str_detect(.$trait_vietnamese, "Sản lượng lúa hè thu|Sản lượng lúa Hè thu") ~ "Sản lượng lúa hè thu phân theo huyện", 
      TRUE ~ .$trait_vietnamese
    )
  )%>% 
  # replace / unite name of trait (English)
  # This process depends on the Vietnamese-named traits.
  # Do the Vietnamese part first!!
  dplyr::mutate(
    # Let us add conditions below accordingly.
    trait = dplyr::case_when(　　 
      # production of shrimp aquaculture
      stringr::str_detect(.$trait_vietnamese, "Sản lượng tôm nuôi") ~ "production of shrimp aquaculture", 
      # annual production of paddy rice
      stringr::str_detect(.$trait_vietnamese, "Sản lượng lúa cả năm phân theo huyện") ~ "production of paddy rice", 
      # winter spring production of paddy rice
      stringr::str_detect(.$trait_vietnamese, "Sản lượng lúa Đông xuân phân theo huyện") ~ "production of winter spring paddy rice", 
      # summer autumn production of paddy rice
      stringr::str_detect(.$trait_vietnamese, "Sản lượng lúa hè thu phân theo huyện") ~ "production of summer autumn paddy rice", 
      TRUE ~ .$trait
    )
  )%>% 
  # omit NA trait
  # When the original data does not have any English title,
  # The function above returns NA.
  # Until the English name will be clarified, we omit such a data.
  # When it will be clarified, we will be able to add the data.
  dplyr::filter(!is.na(trait)) %>% 
  # reshape the read tables
  dplyr::mutate(
    rawdata = purrr::map(
      .$rawdata,
      ~
        # remove the first, second, and third rows.
        # In the rows, we do not find any useful information.
        dplyr::slice(.,-c(1:3)) %>%
        # remove columns without any data
        dplyr::select_if(
          ~ !all(is.na(.))
          ) %>% 
        # add a column name to the first column
        # Adding that, we can use the next function.
        # In the beginning, R provides names automatically to the data 
        # generated above. The name, "...1", is an example.
        # The generated names are not appropriate for future analysis.
        # Besides, in [1,1], because of constraint of original data, 
        # R returns NA to the element [1,1]. The NA often 
        # results in malfunction. So we should replace
        # the element [1,1] into an appropriate content.
        # The first column ([,1]), represents names of 
        # district. We add "district" to the element.
        tidyr::replace_na(
          list(...1  = "district")
          ) %>%
        # use the first row as columns' names
        janitor::row_to_names(row_number = 1) %>% 
        # reshape the data into tidy form
        tidyr::pivot_longer(
          data= .,
          # omit the column named "district"
          - district,
          names_to = "year",
          values_to = "number"
          ) %>% 
        dplyr::mutate(
          district = factor(district),
          year = lubridate::ymd(paste0(as.numeric(year),"0101")),
          number = tidyr::replace_na(as.numeric(number),0)
        ) %>% 
        dplyr::filter(
          !(stringr::str_detect(
            district, 
            "TOTAL|TỔNG SỐ|Tổng số|Total"
            )
            )
          ) %>% 
        dplyr::mutate(
          district = dplyr::case_when(
            # Bac Lieu (7)
            stringr::str_detect(district, "Bac Lieu") ~ "Bac Lieu", #
            stringr::str_detect(district, "Dong Hai") ~ "Dong Hai", #
            stringr::str_detect(district, "Gia Rai") ~ "Gia Rai", #
            stringr::str_detect(district, "Hong Dan") ~ "Hong Dan", #
            stringr::str_detect(district, "Hoa Binh") ~ "Hoa Binh", #
            stringr::str_detect(district, "Phuoc Long") ~ "Phuoc Long", #
            stringr::str_detect(district, "Vinh Loi") ~ "Vinh Loi", #
            # Ben Tre (8)
            stringr::str_detect(district, "Ben Tre") ~ "Ben Tre", #
            stringr::str_detect(district, "Ba Tri") ~ "Ba Tri", #
            stringr::str_detect(district, "Binh Dai") ~ "Binh Dai", #
            stringr::str_detect(district, "Cho Lach") ~ "Cho Lach", #
            stringr::str_detect(district, "Chau Thanh") ~ "Chau Thanh", #
            stringr::str_detect(district, "Giong Trom") ~ "Giong Trom", #
            # stringr::str_detect(district, "Mo Cay Bac") ~ "Mo Cay Bac",
            # stringr::str_detect(district, "Mo Cay Nam") ~ "Mo Cay Nam",
            stringr::str_detect(district, "Mo Cay") ~ "Mo Cay",  #
            stringr::str_detect(district, "Thanh Phu") ~ "Thanh Phu", #
            # Ca Mau (9)
            stringr::str_detect(district, "Ca Mau") ~ "Ca Mau", #
            stringr::str_detect(district, "Cai Nuoc") ~ "Cai Nuoc", #
            stringr::str_detect(district, "Dam Doi") ~ "Dam Doi", #
            stringr::str_detect(district, "Nam Can") ~ "Nam Can", #
            stringr::str_detect(district, "Ngoc Hien") ~ "Ngoc Hien", #
            stringr::str_detect(district, "Phu Tan") ~ "Phu Tan", #
            stringr::str_detect(district, "Thoi Binh") ~ "Thoi Binh", #
            stringr::str_detect(district, "Tran V. Thoi|Tran Van Thoi") ~ "Tran Van Thoi", #
            stringr::str_detect(district, "U Minh rural") ~ "U Minh", #
            # Kien Giang (15)
            stringr::str_detect(district, "An Bien|An Biên") ~ "An Bien", #
            stringr::str_detect(district, "An Minh|An Minh") ~ "An Minh", #
            stringr::str_detect(district, "Giong Rieng|Giồng Riềng") ~ "Giong Rieng", #
            stringr::str_detect(district, "Giang Thanh") ~ "Giang Thanh", 
            stringr::str_detect(district, "Go Quao|Huyện Gò Quao") ~ "Go Quao", #
            stringr::str_detect(district, "Ha Tien|Hà Tiên") ~ "Ha Tien", #
            stringr::str_detect(district, "Hon Dat|Hòn Đất") ~ "Hon Dat", #
            stringr::str_detect(district, "Kien Hai|Kiên Hải") ~ "Kien Hai", #
            stringr::str_detect(district, "Kien Luong|Kiên Lương") ~ "Kien Luong", #
            stringr::str_detect(district, "Phu Quoc|Phú Quốc") ~ "Phu Quoc", #
            stringr::str_detect(district, "Rach Gia|Rạch Giá") ~ "Rach Gia", #
            stringr::str_detect(district, "Tan Hiep|Tân Hiệp") ~ "Tan Hiep", #
            stringr::str_detect(district, "U Minh Thượng") ~ "U Minh Thuong", #
            stringr::str_detect(district, "Vinh Thuan|Vĩnh Thuận") ~ "Vinh Thuan", #
            stringr::str_detect(district, "Chau Thanh|Huyện Châu Thành") ~ "Chau Thanh", #
            # Soc Trang (10)
            stringr::str_detect(district, "Cu Lao Dung") ~ "Cu Lao Dung", #
            stringr::str_detect(district, "Ke Sach") ~ "Ke Sach", #
            stringr::str_detect(district, "Long Phu") ~ "Long Phu", #
            stringr::str_detect(district, "My Tu") ~ "My Tu",#
            stringr::str_detect(district, "My Xuyen") ~ "My Xuyen", #
            stringr::str_detect(district, "Nga Nam") ~ "Nga Nam", #
            stringr::str_detect(district, "Soc Trang") ~ "Soc Trang", #
            stringr::str_detect(district, "Thanh Tri") ~ "Thanh Tri", #
            stringr::str_detect(district, "Tran De") ~ "Tran De", 
            stringr::str_detect(district, "Vinh Chau") ~ "Vinh Chau", #
            # Tra Vinh (8)
            stringr::str_detect(district, "Cau Ke") ~ "Cau Ke", #
            stringr::str_detect(district, "Cau Ngang") ~ "Cau Ngang", #
            stringr::str_detect(district, "Cang Long") ~ "Cang Long", #
            stringr::str_detect(district, "Huyện Châu Thành - Chau Thanh District") ~ "Chau Thanh", #
            stringr::str_detect(district, "Duyen Hai") ~ "Duyen Hai", #
            stringr::str_detect(district, "Tieu Can") ~ "Tieu Can", #
            stringr::str_detect(district, "Tra Cu") ~ "Tra Cu", #
            stringr::str_detect(district, "Tra Vinh") ~ "Tra Vinh", #
            TRUE ~ "hoge"
          )
        # ) %>% 
        # dplyr::mutate(
        #   Province = dplyr::case_when(
        #     stringr::str_detect(district, "Bac Lieu|Dong Hai|Gia Rai|Hong Dan|Hoa Binh|Phuoc Long|Vinh Loi") ~ "Bac Lieu",
        #     stringr::str_detect(district, "Ben Tre|Ba Tri|Binh Dai|Cho Lach|Chau Thanh (BT)|Giong Trom|Mo Cay|Mo Cay Bac|Mo Cay Nam|Thanh Phu") ~ "Ben Tre",
        #     stringr::str_detect(district, "Ca Mau|Cai Nuoc|Dam Doi|Nam Can|Ngoc Hien|Phu Tan|Thoi Binh|Tran Van Thoi|(^U Minh$)") ~ "Ca Mau",
        #     stringr::str_detect(district, "An Bien|An Minh|Chau Thanh (KG)|Giong Rieng|Giang Thanh|Go Quao|Ha Tien|Hon Dat|Kien Hai|Kien Luong|Phu Quoc|Rach Gia|Tan Hiep|(^U Minh Thuong$)|Vinh Thuan") ~ "Kien Giang",
        #     stringr::str_detect(district, "Cu Lao Dung|Ke Sach|Long Phu|My Tu|My Xuyen|Nga Nam|Soc Trang|Thanh Tri|Tran De|Vinh Chau") ~ "Soc Trang",
        #     stringr::str_detect(district, "Cau Ke|Cau Ngang|Cang Long|Chau Thanh (TV)|Duyen Hai|Duyen Hai|Tieu Can|Tra Cu|Tra Vinh") ~ "Tra Vinh",
        #     TRUE ~ "NA"
        #   )
        )
      )
    ) %>% 
  # 
  dplyr::mutate(
    number_row = purrr::map(
      .$rawdata,
      ~
        nrow(.)
    ) 
  ) %>% 
  dplyr::mutate(number_row = as.numeric(unlist(.$number_row)))
# make a data frame for convenience
nlnn_mekong_df <- 
  data.frame(
    rawdata.df = do.call(function(...) rbind(data.frame(), ...), nlnn_mekong$rawdata),
    province_en = rep(nlnn_mekong$province, times = nlnn_mekong$number_row),
    trait_en = rep(nlnn_mekong$trait, times = nlnn_mekong$number_row),
    trait_vn = rep(nlnn_mekong$trait_vietnamese, times = nlnn_mekong$number_row)
  ) %>% 
  data.table::setnames(
    c(
      "district",
      "year",
      "number",
      "province_en",
      "trait_en",
      "trait_vn"
    )
  ) %>% 
  dplyr::mutate(
    province_vn = dplyr::case_when(
      stringr::str_detect(province_en, "Bac Lieu") ~ "Bạc Liêu",
      stringr::str_detect(province_en, "Ben Tre") ~ "Bến Tre",
      stringr::str_detect(province_en, "Ca Mau") ~ "Cà Mau",
      stringr::str_detect(province_en, "Kien Giang") ~ "Kiên Giang",
      stringr::str_detect(province_en, "Soc Trang") ~ "Sóc Trăng",
      stringr::str_detect(province_en, "Tra Vinh") ~ "Trà Vinh",
      TRUE ~ "NA"
    )
  ) %>% 
  dplyr::na_if(.,0) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(
    dplyr::across(
      .cols = c(district, province_en, province_vn, trait_vn), 
      .fns = factor
      )
    )
# save the data frame
nlnn_mekong_df %>%
  readr::write_excel_csv("nlnn_mekong_df.csv")

# 
##
### END ### ---

##### ---- figure.collection ---- 
# make a subset of the data set
nlnn_mekong_df_target <- 
  nlnn_mekong_df %>% 
  dplyr::filter(
    trait_en %in% c(
      "production of shrimp aquaculture", 
      "production of paddy rice", 
      "production of winter spring paddy rice", 
      "production of summer autumn paddy rice"
      )
    )
# 1. annual paddy rice production by province and district
nlnn_mekong_line_paddy_total <-
  nlnn_mekong_df_target %>% 
  dplyr::filter(trait_en == "production of paddy rice") %>% 
  group_by(province_en) %>%
  nest() %>% 
  dplyr::mutate(
    line_plot = purrr::map(
      data, 
      ~
        ggplot2::ggplot(
          data = .,
          aes(
            x = year, 
            y = number,
            color = district
          )
        ) +
        geom_line() +
        geom_point() + 
        scale_color_discreterainbow() +
        xlim(
          as.Date("2000/01/01"),
          as.Date("2010/01/01")
          ) +
        ylim(0, 600000) +
        labs(
          title = province_en,
          x = "Year",
          y = "Paddy rice production (Unit: Ton)",
          color = "District"
        ) +
        guides(color = guide_legend(title.position = "top", ncol = 2)) +
        theme_classic() +
        theme(
          legend.position = "none",
          legend.text = element_text(size = 10)
        )
    )
  )
# adjust legend position and other miscellaneous settings
# to meet A4 paper size
# 
# Bac Lieu
nlnn_paddy_line_01 <- nlnn_mekong_line_paddy_total$line_plot[[1]] + theme(legend.position = c(0.5,0.85), axis.title.x=element_blank())
# Ben Tre
nlnn_paddy_line_02 <- nlnn_mekong_line_paddy_total$line_plot[[2]] + theme(legend.position = c(0.5,0.85), axis.title.y=element_blank())
# Ca Mau
nlnn_paddy_line_03 <- 
  nlnn_mekong_line_paddy_total$line_plot[[3]] + 
  theme(
    legend.position = c(0.5,0.85), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank()
    )
# Kien Giang
nlnn_paddy_line_04 <- 
  nlnn_mekong_line_paddy_total$line_plot[[4]] + 
  theme(
    legend.position = c(0.175,0.6), 
    legend.key = element_blank(), 
    legend.background = element_blank(), 
    legend.text = element_text(size = 8),
    axis.title.x=element_blank()
    ) + 
  guides(
    color = guide_legend(
      title.position = "top", 
      ncol = 1, 
      override.aes=list(fill=NA)
      )
    ) 
# Soc Trang
nlnn_paddy_line_05 <- 
  nlnn_mekong_line_paddy_total$line_plot[[5]] + 
  theme(
    legend.position = c(0.5,0.85), 
    legend.key = element_blank(), 
    legend.background = element_blank(), 
    legend.text = element_text(size = 8),
    axis.title.y=element_blank()
    )
# Tra Vinh
nlnn_paddy_line_06 <- nlnn_mekong_line_paddy_total$line_plot[[6]] + theme(legend.position = c(0.5,0.85), axis.title.x=element_blank(), axis.title.y=element_blank())
# combine the figures of 6 provinces using patchwork() library
nlnn_mekong_line_paddy_total <- 
(nlnn_paddy_line_01 + nlnn_paddy_line_02 + nlnn_paddy_line_03) / (nlnn_paddy_line_04 + nlnn_paddy_line_05 + nlnn_paddy_line_06)
# save the combined figure
ggsave(
  "nlnn_mekong_line_paddy_total.pdf",
  plot = nlnn_mekong_line_paddy_total,
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
# 2. annual shrimp aquaculture production by province and district
nlnn_mekong_line_shrimp_total <-
  nlnn_mekong_df_target %>% 
  dplyr::filter(trait_en == "production of shrimp aquaculture") %>% 
  group_by(province_en) %>%
  nest() %>% 
  dplyr::mutate(
    line_plot = purrr::map(
      data, 
      ~
        ggplot2::ggplot(
          data = .,
          aes(
            x = year, 
            y = number,
            color = district
          )
        ) +
        geom_line() +
        geom_point() + 
        scale_color_discreterainbow() +
        xlim(
          as.Date("2000/01/01"),
          as.Date("2010/01/01")
        ) +
        ylim(0, 30000) +
        labs(
          title = province_en,
          x = "Year",
          y = "Shrimp aquaculture production (Unit: Ton)",
          color = "District"
        ) +
        guides(color = guide_legend(title.position = "top", ncol = 2)) +
        theme_classic() +
        theme(
          legend.position = "none",
          legend.text = element_text(size = 10)
        )
    )
  )
# adjust legend position and other miscellaneous settings
# to meet A4 paper size
# 
# Bac Lieu
nlnn_shrimp_line_01 <- 
  nlnn_mekong_line_shrimp_total$line_plot[[1]] + 
  theme(
    legend.position = c(0.75,0.85), 
    legend.key = element_blank(), 
    legend.background = element_blank(), 
    legend.text = element_text(size = 10),
    axis.title.x=element_blank()
  ) + 
  guides(
    color = guide_legend(
      title.position = "top", 
      ncol = 2, 
      override.aes=list(fill=NA)
    )
  ) 
# Ben Tre
nlnn_shrimp_line_02 <- nlnn_mekong_line_shrimp_total$line_plot[[2]] + theme(legend.position = c(0.5,0.85), axis.title.y=element_blank())
# Ca Mau
nlnn_shrimp_line_03 <- 
  nlnn_mekong_line_shrimp_total$line_plot[[3]] + 
  theme(
    legend.position = c(0.725,0.55), 
    axis.title.x=element_blank(), 
    legend.key = element_blank(), 
    legend.background = element_blank(), 
    legend.text = element_text(size = 10)
  ) + 
  guides(
    color = guide_legend(
      title.position = "top", 
      ncol = 2, 
      override.aes=list(fill=NA)
    )
  )
# Kien Giang
nlnn_shrimp_line_04 <- 
  nlnn_mekong_line_shrimp_total$line_plot[[4]] + 
  theme(
    legend.position = c(0.2,0.5), 
    legend.key = element_blank(), 
    legend.background = element_blank(), 
    legend.text = element_text(size = 10),
    axis.title.x=element_blank()
  ) + 
  guides(
    color = guide_legend(
      title.position = "top", 
      ncol = 1, 
      override.aes=list(fill=NA)
    )
  ) 
# Soc Trang
nlnn_shrimp_line_05 <- 
  nlnn_mekong_line_shrimp_total$line_plot[[5]] + 
  theme(
    legend.position = c(0.2,0.5), 
    legend.key = element_blank(), 
    legend.background = element_blank(), 
    legend.text = element_text(size = 10),
    axis.title.y=element_blank()
  ) +
  guides(
    color = guide_legend(
      title.position = "top", 
      ncol = 1, 
      override.aes=list(fill=NA)
    )
  ) 
# Tra Vinh
nlnn_shrimp_line_06 <- 
  nlnn_mekong_line_shrimp_total$line_plot[[6]] + 
  theme(
    legend.position = c(0.2,0.5), 
    axis.title.x=element_blank(), 
    axis.title.y=element_blank()
    ) +
  guides(
    color = guide_legend(
      title.position = "top", 
      ncol = 1, 
      override.aes=list(fill=NA)
    )
  ) 

# combine the figures of 6 provinces using patchwork() library
nlnn_mekong_line_shrimp_total <- 
  (nlnn_shrimp_line_01 + nlnn_shrimp_line_02 + nlnn_shrimp_line_03) / (nlnn_shrimp_line_04 + nlnn_shrimp_line_05 + nlnn_shrimp_line_06)
# save the combined figure
ggsave(
  "nlnn_mekong_line_shrimp_total.pdf",
  plot = nlnn_mekong_line_shrimp_total,
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
### END ### ---
##
#

##### ---- union.map ---- 
# We need to unite district-level administrative boundaries to
# cope with merge / establishment of district in 2009.
# In BT, KG, and ST, some communes have been separated to deal with overpopulation.
# This results in rearrangement of the administrative boundaries. It means that
# we need to recompute spatial weight matrix in accordance with the rearrangement.
# The administrative boundaries data, however, is not distributed at the moment.
# We need to make the data by ourselves using existing data.
# 
# Referring to the MSExcel data set, we can check existed districts' names by province.
# The following three districts were established in 2009
# 1/3 MO CAY in BT were then separated into two districts; MO CAY BAC and MO CAY NAM
# In this case, we just need to unite the MCB and MCN.
# 
# 2/3 KIEN LUONG in KG were then separated into two districts; GIANG THANH AND KIEN LUONG 
# In this case, as in Mo Cay, we need to unite GT and KL.
# 
# 3/3 TRAN DE in ST were then established from communes of two districts; LONG PHU AND MY XUYEN
# In this case, the situation appears complex.
# The Tran De district were established from communes belonging Long Phu and My Xuyen. 
# To make existed administrative boundaries of Long Phu and My Xuyen, we need to 
# separate communes belonging to Tran De while checking original affiliation. At the moment,
# there exists 11 communes as follows:
# 
# 1. Dai An 2: former a part of Long Phu
# 2. Lich Hoi Thuong: former a part of Long Phu
# 3. Lich Hoi Thuong: former a part of Long Phu
# 4. Lieu Tu: former a part of Long Phu
# 5. Tai Van: former a part of My Xuyen
# 6. Thanh Thoi An: former a part of My Xuyen
# 7. Thanh Thoi Thuan: former a part of My Xuyen
# 8. Tran De: former a part of Long Phu (newly established in 2009)
# 9. Trung Binh: former a part of Long Phu
# 10. Vien An: former a part of My Xuyen
# 11. Vien Binh: former a part of My Xuyen
# 
# Fortunately, we have commune-level administrative boundaries data. 
# We can!!
# 
# read source of another file to use existing codes
# In the source, there are some codes to draw maps
source("MRDmap.r")
# 
# make a merged polygon excluding target districts
# NOTE
# As for the Tran de district in ST, we use adm3 data.
# Here, we just omit the relevant districts in ST. Afterward, we will combine. 
adm2_mekong_ex_target <- 
  adm2_mekong %>% 
  dplyr::filter(!(NAME_2 %in% c(
    "Mỏ Cày Bắc",
    "Mỏ Cày Nam", 
    "Giang Thành", 
    "Kiên Lương", 
    "Châu Thành", 
    "Mỹ Tú", 
    "Mỹ Xuyên", 
    "Long Phú", 
    "Trần Đề"
    )
    )
    )
# Ben Tre
# make a merged polygon of MO CAY district in BT
mo_cay_bt_sf <- 
  adm2_mekong %>% 
  dplyr::filter(
    # pick up target district
    NAME_2 %in% c(
      "Mỏ Cày Bắc", 
      "Mỏ Cày Nam"
      )
    ) %>% 
  # unite the picked up district
  sf::st_union(
    by_feature = FALSE
    ) %>% 
  # cast geometry's type to another
  # This time, we transform that into MULTIPOLYGON.
  sf::st_cast("MULTIPOLYGON")
# make a data frame for the MO CAY district in BT
# We set temporal GIDs. In fact, we do not use the GIDs and do not need to
# consider the GIDs carefully.
mo_cay_bt_df <- data.frame(
  GID_0  = "VNM", 
  NAME_0 = "Vietnam", 
  GID_1  = "VNM.6_united",
  NAME_1  = "Bến Tre",
  NL_NAME_1  = NA,
  GID_2  = "VNM.6.7_united",      
  NAME_2  = "Mỏ Cày",  
  VARNAME_2  = "Mo Cay",
  NL_NAME_2  = NA, 
  TYPE_2  = "Huyện", 
  ENGTYPE_2  = "District", 
  CC_2  = NA,   
  HASC_2  = "VN.VL.BM_united"
)
# make a data frame with sf and data.frame objects
# Later, we will combine this with an object named "adm2_mekong_ex_target"
mo_cay_bt <- cbind(mo_cay_bt_df,mo_cay_bt_sf) %>% st_as_sf(sf_column_name = "geometry")
# 
# Kien Giang
# make a merged polygon of Kien Luong district in KG
kien_luong_kg_sf <- 
  adm2_mekong %>% 
  dplyr::filter(
    # pick up target district
    NAME_2 %in% c(
      "Kiên Lương", 
      "Giang Thành"
    )
  ) %>% 
  # unite the picked up district
  sf::st_union(
    by_feature = FALSE
  ) %>% 
  # cast geometry's type to another
  # This time, we transform that into MULTIPOLYGON.
  sf::st_cast("MULTIPOLYGON")
# make a data frame for the Kien Luong district in KG
# We set temporal GIDs. In fact, we do not use the GIDs and do not need to
# consider the GIDs carefully.
kien_luong_kg_df <- data.frame(
  GID_0  = "VNM", 
  NAME_0 = "Vietnam", 
  GID_1  = "VNM.33_united",
  NAME_1  = "Kiên Giang",
  NL_NAME_1  = NA,
  GID_2  = "VNM.33.10_1_united_united",      
  NAME_2  = "Kiên Lương",  
  VARNAME_2  = "Kien Luong",
  NL_NAME_2  = NA, 
  TYPE_2  = "Huyện", 
  ENGTYPE_2  = "District", 
  CC_2  = NA,   
  HASC_2  = "VN.QN.MC_united"
)
# make a data frame with sf and data.frame objects
# Later, we will combine this with an object named "adm2_mekong_ex_target"
kien_luong_kg <- cbind(kien_luong_kg_df,kien_luong_kg_sf) %>% st_as_sf(sf_column_name = "geometry")
# 
# Soc Trang
my_tu_st_sf <- 
  adm2_mekong %>% 
  dplyr::filter(
    # pick up target district
    NAME_2 %in% c(
      "Châu Thành", 
      "Mỹ Tú"
      ) &
    NAME_1 == "Sóc Trăng"
  ) %>% 
  # unite the picked up district
  sf::st_union(
    by_feature = FALSE
  ) %>% 
  # cast geometry's type to another
  # This time, we transform that into MULTIPOLYGON.
  sf::st_cast("MULTIPOLYGON")
# make a data frame for the Kien Luong district in KG
# We set temporal GIDs. In fact, we do not use the GIDs and do not need to
# consider the GIDs carefully.
my_tu_st_df <- data.frame(
  GID_0  = "VNM", 
  NAME_0 = "Vietnam", 
  GID_1  = "VNM.51_1_united",
  NAME_1  = "Sóc Trăng",
  NL_NAME_1  = NA,
  GID_2  = "VNM.51.1_1_united_united",      
  NAME_2  = "Mỹ Tú",  
  VARNAME_2  = "My Tu",
  NL_NAME_2  = NA, 
  TYPE_2  = "Huyện", 
  ENGTYPE_2  = "District", 
  CC_2  = NA,   
  HASC_2  = NA
)
# make a data frame with sf and data.frame objects
# Later, we will combine this with an object named "adm2_mekong_ex_target"
my_tu_st <- cbind(my_tu_st_df,my_tu_st_sf) %>% st_as_sf(sf_column_name = "geometry")
# My Xuyen
my_xuyen_st_sf <- 
  adm3 %>% 
  dplyr::filter(
    # pick up target district
    NAME_2 %in% c(
      "Long Phú", 
      "Mỹ Xuyên",
      "Trần Đề"
    )
  ) %>% 
  dplyr::filter(
    NAME_3 %in% c(
      "Tài Văn",
      "Thạnh Thới An",
      "Thạnh Thới Thuận",
      "Viên An",
      "Viên Bình",
      "Đại Tâm",
      "Gia Hòa 1",
      "Gia Hòa 2",
      "Hòa Tú 1",
      "Hòa Tú 2",
      "Mỹ Xuyên",
      "Ngọc Đông",
      "Ngọc Tố",
      "Thạnh Phú",
      "Thạnh Quới",
      "Tham Đôn"
    )
  ) %>% 
  # unite the picked up district
  sf::st_union(
    by_feature = FALSE
  ) %>% 
  # cast geometry's type to another
  # This time, we transform that into MULTIPOLYGON.
  sf::st_cast("MULTIPOLYGON")
# Soc Trang
# Long Phu
long_phu_st_sf <- 
  adm3 %>% 
  dplyr::filter(
    # pick up target district
    NAME_2 %in% c(
      "Long Phú", 
      "Mỹ Xuyên",
      "Trần Đề"
    )
  ) %>% 
  dplyr::filter(
    NAME_3 %in% c(
      "Đại Ân 2",
      "Lịch Hội Thượng (Thị trấn )",
      "Lịch Hội Thượng (Xã)",
      "Liêu Tú",
      "Trần Đề",
      "Trung Bình",
      "Châu Khánh",
      "Đại Ngãi",
      "Hậu Thạnh",
      "Long Đức",
      "Long Phú (Thị trấn )",
      "Long Phú (Xã)",
      "Phú Hữu",
      "Song Phụng",
      "Tân Hưng",
      "Tân Thạnh",
      "Trường Khánh"
    )
  ) %>% 
  # unite the picked up district
  sf::st_union(
    by_feature = FALSE
  ) %>% 
  # cast geometry's type to another
  # This time, we transform that into MULTIPOLYGON.
  sf::st_cast("MULTIPOLYGON")
my_xuyen_st_df <- data.frame(
  GID_0  = "VNM", 
  NAME_0 = "Vietnam", 
  GID_1  = "VNM.51_united",
  NAME_1  = "Sóc Trăng",
  NL_NAME_1  = NA,
  GID_2  = "VNM.51.6_1_united",      
  NAME_2  = "Mỹ Xuyên",  
  VARNAME_2  = "My Xuyen",
  NL_NAME_2  = NA, 
  TYPE_2  = "Huyện", 
  ENGTYPE_2  = "District", 
  CC_2  = NA,   
  HASC_2  = "VN.HD.TM_united"
)
long_phu_st_df <- data.frame(
  GID_0  = "VNM", 
  NAME_0 = "Vietnam", 
  GID_1  = "VNM.51_united",
  NAME_1  = "Sóc Trăng",
  NL_NAME_1  = NA,
  GID_2  = "VNM.51.4_1_united",      
  NAME_2  = "Long Phú",  
  VARNAME_2  = "Long Phu",
  NL_NAME_2  = NA, 
  TYPE_2  = "Huyện", 
  ENGTYPE_2  = "District", 
  CC_2  = NA,   
  HASC_2  = "VN.DA.TK_united"
)
# 
# make a data frame with sf and data.frame objects
# Later, we will combine this with an object named "adm2_mekong_ex_target"
my_xuyen_st <- cbind(my_xuyen_st_df,my_xuyen_st_sf) %>% st_as_sf(sf_column_name = "geometry")
long_phu_st <- cbind(long_phu_st_df,long_phu_st_sf) %>% st_as_sf(sf_column_name = "geometry")
# 
# Combine the data altogether
adm2_mekong_before_2009 <- 
  adm2_mekong_ex_target %>% 
  dplyr::bind_rows(mo_cay_bt) %>% 
  dplyr::bind_rows(kien_luong_kg) %>%
  dplyr::bind_rows(my_tu_st) %>% 
  dplyr::bind_rows(my_xuyen_st) %>% 
  dplyr::bind_rows(long_phu_st) 
# merge the combined data with sf data
nlnn_mekong_df_target_sf <- 
  nlnn_mekong_df_target %>% 
  dplyr::left_join(adm2_mekong_before_2009,
                   by = c(
                     "province_vn" = "NAME_1", 
                     "district" = "VARNAME_2"
                     )
  ) %>% 
  dplyr::select(
    -GID_0, 
    -NAME_0, 
    -NL_NAME_1, 
    -GID_1, 
    -GID_2, 
    -NL_NAME_2, 
    -TYPE_2, 
    -ENGTYPE_2, 
    -CC_2, 
    -HASC_2
    ) %>% 
  data.table::setnames(
    c(
      "district_en",
      "year",
      "number",
      "province_en",
      "trait_en",
      "trait_vn",
      "province_vn",
      "district_vn",
      "geometry"
      )
    )
# 
# Draw choropleth maps by year and trait
nlnn_mekong_df_target_map <- 
  nlnn_mekong_df_target_sf %>% 
  group_by(year, trait_en) %>% 
  nest() %>% 
  dplyr::mutate(
    production_map = purrr::map(
      data,
      ~ 
        ggplot(data = ., aes(geometry = geometry, fill = number)) +
        geom_sf() + 
        scale_fill_viridis(option = "plasma", na.value = "white") +
        labs(
          title = trait_en, 
          subtitle = year,
          x = "Longitude",
          y = "Latitude",
          fill = "Production volume \n (Unit: metric t)"
          ) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.key.width= unit(2, 'cm')
        )
    )
  )
# save the map
pdf("nlnn_mekong_df_target_map.pdf")
nlnn_mekong_df_target_map$production_map
dev.off()
# 
# 
##
### END ### ---
##
#
