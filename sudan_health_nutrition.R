# Sudan Maternal and Child Health and Nutrition Survey 2018 --------------------


## Load packages in packages.R and project-specific functions in R folder ---- 
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


## Read data ----
maternal <- read.csv("data/maternal_health.csv")
child <- read.csv("data/child_health.csv")
cmam <- read.csv("data/cmam_routine_data.csv")

### Retrieve and read Sudan map data ----
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")
sudan_map_url <- "https://github.com/spatialworks/sudan/raw/master/data-raw/maps/sudan.gpkg"

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_url, layer = "state")
sudan2 <- st_read(dsn = sudan_map_url, layer = "locality")

### Create supporting data dictionaries for each data source ----
child_dictionary <- create_child_dictionary(child, keep = TRUE)
maternal_dictionary <- create_maternal_dictionary(maternal, keep = TRUE)
cmam_dictionary <- create_cmam_dictionary(cmam, keep = TRUE)


## Source the different data processing and analysis workflow steps ----

source("sudan_health_nutrition_1.R")
source("sudan_health_nutrition_2.R")
source("sudan_health_nutrition_3.R")
source("sudan_health_nutrition_4.R")
source("sudan_health_nutrition_5.R")
source("sudan_health_nutrition_6.R")

