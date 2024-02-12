# Load R package dependencies --------------------------------------------------

# Setup the workflow environmen ----

## Load packages in packages.R and project-specific functions in R folder ---- 
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


## Read data ----
maternal <- read.csv("data/maternal_health.csv")
child <- read.csv("data/child_health.csv")
cmam <- read.csv("data/cmam_routine_data.csv")

### Retrieve and read Sudan map data ----
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])

## Run the specific workflow ---
source("sudan_health_nutrition_1.R")

install.packages("rgdal")
## Load general use packages ----
library(openxlsx)     ## Read and write XLSX files
library(dplyr)        ## Data wrangling and manipulation using tidy approach
library(tidyr)        ## Data wrangling and manipulation using tidy approach
library(ggplot2)      ## Data visualisation using tidy approach
library(rmarkdown)    ## Literate programming and report generation
library(remotes)      ## For installing packages from GitHub
library(here)         ## Utility package for working directory helper


## Load packages for mapping ----
library(sf)
library(tmap) ## Main package for mapping
# Use dplyr package
library(dplyr)

#Load child nutrition data 
child <- read.csv("data/child_health.csv")

#Group children by wasted or not wasted 
#group by states
#calculate the percentage of wasted children in each state
#merge the states data with a map
#Use spatial distribution or heat areas in the map

# Create a new variable to classify children based on wasting status
child_wasting <- child %>%
  mutate(wasting_status = ifelse(whz < -2, 1, 0))

# Group children by state_name and calculate % wasted children per state
state_summary <- child_wasting %>%
  group_by(state_name) %>%
  summarise(
    total_child = n(),                                       # Count total children
    wasted_child = sum(wasting_status == 1, na.rm = TRUE),   # Count wasted children
    percentage_undernourished = ifelse(total_child == 0, 0, (wasted_child / total_child) * 100)
  )

# Retrieve and read Sudan map data
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])

# Merge map data with state summary data
merged_data <- merge(sudan2, state_summary, by.x = "admin1Name_en", by.y = "state_name")

# Calculate Percentage of Undernourished Mothers
merged_data <- merged_data %>% 
  mutate(percentage_undernourished = ifelse(total_child == 0, 0, (wasted_child / total_child) * 100))

# Plot the map using ggplot2
ggplot() +
  geom_sf(data = merged_data, aes(fill = percentage_undernourished)) +
  scale_fill_gradient(name = "Percentage of Wasted Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of Wasted Children by States") +
  theme_minimal()