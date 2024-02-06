## Load general use packages
install.packages(c("openxlsx", "dplyr", "tidyr", "ggplot2", "rmarkdown", "remotes", "here", "sf"))

library(openxlsx)     ## Read and write XLSX files
library(dplyr)        ## Data wrangling and manipulation using tidy approach
library(tidyr)        ## Data wrangling and manipulation using tidy approach
library(ggplot2)      ## Data visualisation using tidy approach
library(rmarkdown)    ## Literate programming and report generation
library(remotes)      ## For installing packages from GitHub
library(here)         ## Utility package for working directory helper


## Load packages for mapping ----
library(sf)           ## Main package for mapping

## extra packages
install.packages(c("rnaturalearth", "rnaturalearthdata"))

library(rnaturalearth)
library(rnaturalearthdata)

install.packages("tidyverse")
library(tidyverse)

## Read data ----
maternal <- read.csv("data/maternal_health.csv")
child <- read.csv("data/child_health.csv")
cmam <- read.csv("data/cmam_routine_data.csv")

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


#read data
spatial_data <- st_read("data/child_health.csv")


#summary
summary(spatial_data)

# Create binary variables for stunting, severe stunting, wasting, severe wasting, underweight, and severe underweight

child$stunting <- ifelse(child$haz <- 2, 1, 0)

child$severe_stunting <- ifelse(child$haz < -3, 1, 0)

child$wasting <- ifelse(child$whz < -2 | child$muac < 125 | child$oedema == "Yes", 1, 0)
child$severe_wasting <- ifelse(child$whz < -3 | child$muac < 115 | child$oedema == "Yes", 1, 0)

child$underweight <- ifelse(child$waz < -2, 1, 0)
child$severe_underweight <- ifelse(child$waz < -3, 1, 0)

# Explore the new variables
summary(child[c("stunting", "severe_stunting", "wasting", "severe_wasting", "underweight", "severe_underweight")])

# Create a new column for undernutrition classification
child$undernutrition_classification <- ifelse(
  child$severe_stunting == 1 | child$severe_wasting == 1 | child$severe_underweight == 1,
  "Severe Undernutrition",
  ifelse(
    child$stunting == 1 | child$wasting == 1 | child$underweight == 1,
    "Undernutrition",
    "No Undernutrition"
  )
)

# Explore the new classification variable
table(child$undernutrition_classification)

# dataset for "maternal"

# Create a new column for maternal underweight classification
maternal$underweight_classification <- ifelse(maternal$muac < 22, "Underweight", "Not Underweight")

# Explore the new classification variable
table(maternal$underweight_classification)

# If you want a separate column indicating individuals who are underweight
maternal$underweight_indicator <- ifelse(maternal$muac < 22, 1, 0)

# Explore the new indicator variable
table(maternal$underweight_indicator)


# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter Sudan from world map data
sudan <- subset(world, admin == "Sudan")

# Create counts for child and maternal data
child_counts <- child %>% group_by(locality_name, undernutrition_classification) %>% summarize(count = n())
maternal_counts <- maternal %>% group_by(locality_name, underweight_classification) %>% summarize(count = n())

# Merge counts with Sudan map data
child_map <- left_join(sudan, child_counts, by = c("admin" = "locality_name"))
maternal_map <- left_join(sudan, maternal_counts, by = c("admin" = "locality_name"))

# Plot the data
ggplot() +
  geom_sf(data = child_map, aes(fill = count), size = 0.5) +
  labs(title = "Number of Children with Undernutrition in Sudan by Locality",
       subtitle = "Fill legend: Count of Children with Undernutrition",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  scale_fill_gradient(low = "green", high = "red")

# Repeat the same for maternal data
ggplot() +
  geom_sf(data = maternal_map, aes(fill = count), size = 0.5) +
  labs(title = "Number of Maternal Individuals with Underweight in Sudan by Locality",
       subtitle = "Fill legend: Count of Maternal Individuals with Underweight",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  scale_fill_gradient(low = "green", high = "red")