# Sudan Maternal and Child Health and Nutrition Survey 2018 --------------------


## Load packages in packages.R and project-specific functions in R folder ---- 
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


## Read data ----
maternal <- read.csv("data/maternal_health.csv")
child <- read.csv("data/child_health.csv")
viewcmam <- read.csv("data/cmam_routine_data.csv")

### Retrieve and read Sudan map data ----

sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])


## Source the different data processing and analysis workflow steps ----

source("sudan_health_nutrition_1.R")
source("sudan_health_nutrition_2.R")
source("sudan_health_nutrition_3.R")
source("sudan_health_nutrition_4.R")
source("sudan_health_nutrition_5.R")
source("sudan_health_nutrition_6.R")

View(child)
dim(child)


#################Create a table of na values per vairable####################

# create a loop to determine number of na values per variabel
na_list<-c()
for (i in 1:ncol(child)) {
na_count<- sum(is.na(child[,i])) 
print(na_count)
na_list<-c(na_list,na_count)
}

colname_list<-colnames(child)

##create a table of na values
na_tbl <- data.frame(colname_list, na_list)

##naming cols
colnames(na_tbl) = c("variable", "values missing") 

##calc percentage of na variables
na_tbl[,3]<-round( na_tbl$`values missing`/nrow(child) *100,2)


##naming cols
colnames(na_tbl) = c("variable", "values missing","%")

View(na_tbl)
