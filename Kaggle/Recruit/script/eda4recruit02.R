setwd("~/work/hana_train/Kaggle/Recruit/script")
list.files()

#=== library ===
library(tidyverse)
library(stringr)
library(lubridate)

#=== sample submit ===
# =================
# Submission File
# For every store and date combination in the test set, submission files should contain two columns: id and visitors.  
# The id is formed by concatenating the air_store_id and visit_date with an underscore. 
# The file should contain a header and have the following format:
sample_sub <- readr::read_csv("../input/sample_submission.csv") 
sample_sub_processed <- sample_sub %>% 
  dplyr::mutate(date= (str_split(id, "_", simplify=TRUE))[,3]) %>% 
  dplyr::mutate(store_id= str_replace_all(id, paste0("_",date), "")) %>% 
  dplyr::mutate(date=ymd(date)) %>% 
  dplyr::mutate(store_type= (str_split(id, "_", simplify=TRUE))[,1]) %>% 
  dplyr::select(store_id, store_type, date, visitors)

sample_sub_processed %>% glimpse() #32,019
sample_sub_processed %>% dplyr::filter(store_type=="air") %>% nrow() #all is "air"
sample_sub_processed %>% dplyr::distinct(store_id) %>% nrow() #must have 821 stores
sample_sub_processed %>% dplyr::group_by(date) %>% dplyr::summarise(day_count=n()) %>% .$date #2017-04-23_2017-05-31
sample_sub_processed %>% dplyr::group_by(date) %>% dplyr::summarise(day_count=n()) %>% dplyr::distinct(day_count)

#=== load csv ===
# =================
store_id_relation <- readr::read_csv("../input/store_id_relation.csv")
air_visit_data <- readr::read_csv("../input/air_visit_data.csv")
date_info <- readr::read_csv("../input/date_info.csv")
hpg_reserve <- readr::read_csv("../input/hpg_reserve.csv")
air_reserve <- readr::read_csv("../input/air_reserve.csv")
air_store_info <- readr::read_csv("../input/air_store_info.csv")
hpg_store_info <- readr::read_csv("../input/hpg_store_info.csv")
# ================= 

#=== glimpse ===
# =================
store_id_relation %>% glimpse() #150
air_visit_data %>% glimpse() #252,108
date_info %>% glimpse() #517
hpg_reserve %>% glimpse() #2,000,320
air_reserve %>% glimpse() #92,378
air_store_info %>% glimpse() #829
hpg_store_info %>% glimpse() #4,690
# =================

#=== JOIN air and hpg ===
join_air_hpg <- dplyr::inner_join(store_id_relation, hpg_store_info, by=c("hpg_store_id"))
join_air_hpg %>% glimpse() #63



# weekday/holiday/preholiday/weekend/preweekend

