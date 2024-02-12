# Spatial representation Sudan project ----

## Load general use packages ----
## install.packages(c("openxlsx", "dplyr", "tidyr", "ggplot2", "rmarkdown", "remotes", "here", "sf"))

library(openxlsx)     ## Read and write XLSX files
library(dplyr)        ## Data wrangling and manipulation using tidy approach
library(tidyr)        ## Data wrangling and manipulation using tidy approach
library(ggplot2)      ## Data visualisation using tidy approach
library(rmarkdown)    ## Literate programming and report generation
library(remotes)      ## For installing packages from GitHub
library(here)         ## Utility package for working directory helper
library(sf)           ## For reading maps


## Read data ----
maternal <- read.csv("data/maternal_health.csv")
child <- read.csv("data/child_health.csv")
cmam <- read.csv("data/cmam_routine_data.csv")


### Retrieve and read Sudan map data ----
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])


#### Source the different data processing and analysis workflow steps ----

source("sudan_health_nutrition_1.R")
source("sudan_health_nutrition_2.R")
source("sudan_health_nutrition_3.R")
source("sudan_health_nutrition_4.R")
source("sudan_health_nutrition_5.R")
source("sudan_health_nutrition_6.R")

# Stunting is defined as the children with height-for-age Z-score (HAZ) < -2SD
# and severe stunting is defined as the children with HAZ < -3SD

##### Define the function ----
classify_stunting <- function(haz) {
  ifelse(
    haz < -3, "severe stunting",
    ifelse(
      haz < -2, "stunting", "normal"
    )
  )
}

##### Apply the function using mutate ----
child <- child %>%
  mutate(stunting_status = classify_stunting(haz))

##### Group children by state_name ----
state_summary <- child %>%
  group_by(state_name) %>%
  summarise(
    total_children = n(),
    stunted_children = sum(stunting_status == "severe stunting" | stunting_status == "stunting", na.rm = TRUE), 
    percentage_stunted = ifelse(total_children == 0, 0, (stunted_children / total_children) * 100)
  )

##### grouping stunting classes by locality ----
child_map <- child %>% 
  group_by(state_name, stunting_status) %>% 
  summarise(total_children = n()) %>% 
  ungroup()

###### Plotting map data ----
ggplot() +
  geom_sf(data = merged_childmap_data, aes(geometry = Shape, fill = stunting_status)) +
  scale_fill_manual(name = "Percentage of Stunted Children",
                    values = c("severe stunting" = "darkblue", "stunting" = "lightblue", "normal" = "gray50")) +
  labs(title = "Percentage of Stunted Children by States") +
  theme_minimal()

