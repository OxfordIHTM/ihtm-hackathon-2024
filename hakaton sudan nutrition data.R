# Load R package dependencies --------------------------------------------------

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
#install.packages("tmap")
library(tmap)## Main package for mapping
#work plan by the group
#Classify mothers as undernutrition or normal
#group by states
#claculate the percentage of undernutrioned motheres in each state
#merge the states data with a map
#Use spatial distribution or heat areas in the map


# Create a new variable to classify undernutrition based on MUAC
maternal <- maternal %>%
  mutate(nutrition_status = ifelse(muac < 220, 1, 0))

# Group mothers by state_name
state_summary <- maternal %>%
  group_by(state_name) %>%
  summarise(
    total_mothers = n(),
    undernourished_mothers = sum(nutrition_status == 1, na.rm = TRUE), # Count undernourished mothers
    percentage_undernourished = ifelse(total_mothers == 0, 0, (undernourished_mothers / total_mothers) * 100)
  )

# Retrieve and read Sudan map data
sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")
sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])


# Merge map data with state summary data
merged_data <- merge(map_data, state_summary, by.x = "admin1Name_en", by.y = "state_name")

# Calculate Percentage of Undernourished Mothers
merged_data <- merged_data %>% 
  mutate(percentage_undernourished = ifelse(total_mothers == 0, NA, (undernourished_mothers / total_mothers) * 100))

# Plot the map using ggplot2
ggplot() +
  geom_sf(data = merged_data, aes(fill = percentage_undernourished.x)) +
  scale_fill_gradient(name = "Percentage of Undernourished Mothers", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of Undernourished Mothers by States") +
  theme_minimal()
