<<<<<<< HEAD
#######classifying children and mothers nutrition status#######
=======
# Spatial representation Sudan project ----

## Load general use packages ----
## install.packages(c("openxlsx", "dplyr", "tidyr", "ggplot2", "rmarkdown", "remotes", "here", "sf"))
>>>>>>> 8b6073f (Hinata: Updated  code for child spatial representation)

#Adding a new underweight status column to the child data frame
child$underweight_class <- classify_underweight_child(child$waz)

#Adding a new stunting status column to the child data frame
child$stunting_status <- classify_stunting_child(child$haz)

#Adding a new stunting status column to the child data frame
child$wasting <- classify_wasting_child(child$whz, child$muac, child$oedema)

# Create a new variable to classify children based on wasting status
child_wasting <- child %>%
  mutate(wasting_status = ifelse(whz < -2, 1, 0))

<<<<<<< HEAD
# Group children by state_name and calculate % wasted children per state
state_summary <- child_wasting %>%
=======
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
>>>>>>> 8b6073f (Hinata: Updated  code for child spatial representation)
  group_by(state_name) %>%
  summarise(
    total_child = n(),                                       # Count total children
    wasted_child = sum(wasting_status == 1, na.rm = TRUE),   # Count wasted children
    percentage_undernourished = ifelse(total_child == 0, 0, (wasted_child / total_child) * 100)
  )

<<<<<<< HEAD
# Create a new variable to classify mother's nutrition status based on MUAC
maternal$undernut <- classify_undernut_mother(maternal$muac)

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


#########calculating percentage malnourishment by state########

##starting by counting the values of underweight by state

child_map_underweight <- child %>% group_by(state_name,underweight_class,state_id) %>% count() %>% ungroup() 
child_map_stunting <- child %>% group_by(state_name,stunting_status,state_id) %>% count() %>% ungroup() 
child_map_wasting <- child %>% group_by(state_name,wasting,state_id) %>% count() %>% ungroup() 
maternal_map_undernourished <- maternal %>% group_by(state_name, undernut, state_id) %>% count() %>% ungroup()

child_map_underweight <- child_map_underweight %>% group_by(state_id) %>% mutate(undernut_percentages = (get_perc(n)))
child_map_stunting <- child_map_stunting %>% group_by(state_id) %>% mutate(stunting_percentages = (get_perc(n)))
child_map_wasting <- child_map_wasting %>% group_by(state_id) %>% mutate(wasting_percentages = (get_perc(n)))
maternal_map_undernourished <- maternal_map_undernourished %>% group_by(state_id) %>% mutate(undernut_percentages = get_perc(n))


#######merging map data with the data frames###############

merged_childmap_underweight <- merge.data.frame(sudan1, child_map_underweight, by.x = "stateID", by.y = "state_id", all.x = TRUE)
merged_childmap_stunting <- merge.data.frame(sudan1, child_map_stunting, by.x = "stateID", by.y = "state_id", all.x = TRUE)
merged_childmap_wasting <- merge.data.frame(sudan1, child_map_wasting, by.x = "stateID", by.y = "state_id", all.x = TRUE)
merged_maternalmap_undernourished <- merge.data.frame(sudan1, maternal_map_undernourished, by.x = "stateID", by.y = "state_id", all.x = TRUE)

########plotting map data################

#underweight children
=======
##### grouping stunting classes by locality ----
child_map <- child %>% 
  group_by(state_name, stunting_status) %>% 
  summarise(total_children = n()) %>% 
  ungroup()

###### Plotting map data ----
>>>>>>> 8b6073f (Hinata: Updated  code for child spatial representation)
ggplot() +
  geom_sf(data = merged_childmap_underweight, aes(geometry = geom, fill = undernut_percentages))+
  scale_fill_gradient(name = "Percentage of  Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of underweight Children by States") +
  theme_minimal()


#stunted children
ggplot() +
  geom_sf(data = merged_childmap_stunting, aes(geometry = geom, fill = stunting_percentages))+
  scale_fill_gradient(name = "Percentage of stunted Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of stunted Children by States") +
  theme_minimal()

#wasted children
ggplot() +
  geom_sf(data = merged_childmap_wasting, aes(geometry = geom, fill = wasting_percentages))+
  scale_fill_gradient(name = "Percentage of wasted Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of wasted Children by States") +
  theme_minimal()

#undernourished mothers
ggplot() +
  geom_sf(data = merged_maternalmap_undernourished, aes(geometry = geom, fill = undernut_percentages))+
  scale_fill_gradient(name = "Percentage of undernourished mothers", low = "lightgreen", high = "darkgreen", na.value = "gray50") +
  labs(title = "Percentage of undernourished mothers by States") +
  theme_minimal()


#Trying the add state initials to the the states in the map
## your_dataset$new_variable <- substr(your_dataset$existing_variable, 1, 2)

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
