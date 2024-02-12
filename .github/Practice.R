# Sudan Maternal and Child Health and Nutrition Survey 2018 --------------------

## Packages
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(remotes)
library(here)
library(sf)
## loaded packages

library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(remotes)
library(here)
library(sf)

## Read data ----
maternal <- read.csv("data/maternal_health.csv")
child <- read.csv("data/child_health.csv")
cmam <- read.csv("data/cmam_routine_data.csv")

### Retrieve and read Sudan map data ----
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])

### Create supporting data dictionaries for each data source ----
child_dictionary <- create_child_dictionary(child, keep = TRUE)
maternal_dictionary <- create_maternal_dictionary(maternal, keep = TRUE)
cmam_dictionary <- create_cmam_dictionary(cmam, keep = TRUE)

## RASIKA'S CODES

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

### Retrieve and read Sudan map data ----
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
@@ -45,86 +34,43 @@ source("sudan_health_nutrition_4.R")
source("sudan_health_nutrition_5.R")
source("sudan_health_nutrition_6.R")

# Stunting is defined as the children with height-for-age Z-score (HAZ) < -2SD
# and severe stunting is defined as the children with HAZ < -3SD

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
  # Define the function
  classify_stunting <- function(haz) {
    ifelse(
      child$stunting == 1 | child$wasting == 1 | child$underweight == 1,
      "Undernutrition",
      "No Undernutrition"
      haz < -3, "severe stunting",
      ifelse(
        haz < -2, "stunting", "normal"
      )
    )
  }
  
  # Apply the function using mutate
  child <- child %>%
    mutate(stunting_status = classify_stunting(haz))
  
  # Group children by state_name
  state_summary <- child %>%
    group_by(state_name) %>%
    summarise(
      total_children = n(),
      stunted_children = sum(stunting_status == "severe stunting" | stunting_status == "stunting", na.rm = TRUE), 
      percentage_stunted = ifelse(total_children == 0, 0, (stunted_children / total_children) * 100)
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
##grouping stunting classes by locality
child_map <- child %>% 
  group_by(state_name, stunting_status) %>% 
  summarise(total_children = n()) %>% 
  ungroup()

# Plot the data
ggplot() +
  geom_sf(data = child_map, aes(fill = count), size = 0.5) +
  labs(title = "Number of Children with Undernutrition in Sudan by Locality",
       subtitle = "Fill legend: Count of Children with Undernutrition",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  scale_fill_gradient(low = "green", high = "red")

# Repeat the same for maternal data
# Plotting map data
ggplot() +
  geom_sf(data = maternal_map, aes(fill = count), size = 0.5) +
  labs(title = "Number of Maternal Individuals with Underweight in Sudan by Locality",
       subtitle = "Fill legend: Count of Maternal Individuals with Underweight",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  scale_fill_gradient(low = "green", high = "red")
geom_sf(data = merged_childmap_data, aes(geometry = Shape, fill = stunting_status)) +
  scale_fill_manual(name = "Percentage of Stunted Children",
                    values = c("severe stunting" = "darkblue", "stunting" = "lightblue", "normal" = "gray50")) +
  labs(title = "Percentage of Stunted Children by States") +
  theme_minimal()