<<<<<<< HEAD
#######classifying children and mothers nutrition status#######

#Adding a new underweight status column to the child data frame
child$underweight_class <- classify_underweight_child(child$waz)

#Adding a new stunting status column to the child data frame
child$stunting_status <- classify_stunting_child(child$haz)
=======
#library(tidyverse)
#library(dplyr)
#library(ggthemes)
#library(ggplot2)
#library(sf)
#library(openxlsx)   
#library(tidyr)       
#library(rmarkdown)   
#library(remotes)     
#library(here)     
#library(tidyr)     
#library(rmarkdown)    

#functions for classifying underweight children
#Underweight is defined as the children with weight-for-age Z-score (WAZ)
#<−2SD and severe underweight is defined as the children with WAZ <−3SD

classify_underweight_child <- function(waz) {
  ifelse(
    waz <= -2  , "underweight",
    ifelse(
      waz <= -3, "severe underweight", "normal"
    )
  )
}



# Create a new variable to classify undernutrition based on MUAC
maternal <- maternal %>%
  mutate(nutrition_status = ifelse(muac < 220, 1, 0))

#Adding a new column to the child data frame

child$underweight_class <- classify_underweight_child(child$waz)

#grouping underweight classes by locality
##starting by counting the values of underweight by state
>>>>>>> 53f4814 (latest on hinata, push from kelechi)

#Adding a new stunting status column to the child data frame
child$wasting <- classify_wasting_child(child$whz, child$muac, child$oedema)

<<<<<<< HEAD
# Create a new variable to classify children based on wasting status
child_wasting <- child %>%
  mutate(wasting_status = ifelse(whz < -2, 1, 0))
=======
##Getting a percentage from the previous count
get_perc = function(x){
  percentage <- round(x/sum(x)*100,1)
  percentage
}
>>>>>>> 53f4814 (latest on hinata, push from kelechi)

# Group children by state_name and calculate % wasted children per state
state_summary <- child_wasting %>%
  group_by(state_name) %>%
  summarise(
    total_child = n(),                                       # Count total children
    wasted_child = sum(wasting_status == 1, na.rm = TRUE),   # Count wasted children
    percentage_undernourished = ifelse(total_child == 0, 0, (wasted_child / total_child) * 100)
  )

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

##Naemi data
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

<<<<<<< HEAD
child_map_underweight <- child_map_underweight %>% group_by(state_id) %>% mutate(undernut_percentages = (get_perc(n)))
child_map_stunting <- child_map_stunting %>% group_by(state_id) %>% mutate(stunting_percentages = (get_perc(n)))
child_map_wasting <- child_map_wasting %>% group_by(state_id) %>% mutate(wasting_percentages = (get_perc(n)))
maternal_map_undernourished <- maternal_map_undernourished %>% group_by(state_id) %>% mutate(undernut_percentages = get_perc(n))
=======
##merging map data with the child_map data frame
>>>>>>> 53f4814 (latest on hinata, push from kelechi)


#######merging map data with the data frames###############

merged_childmap_underweight <- merge.data.frame(sudan1, child_map_underweight, by.x = "stateID", by.y = "state_id", all.x = TRUE)
merged_childmap_stunting <- merge.data.frame(sudan1, child_map_stunting, by.x = "stateID", by.y = "state_id", all.x = TRUE)
merged_childmap_wasting <- merge.data.frame(sudan1, child_map_wasting, by.x = "stateID", by.y = "state_id", all.x = TRUE)
merged_maternalmap_undernourished <- merge.data.frame(sudan1, maternal_map_undernourished, by.x = "stateID", by.y = "state_id", all.x = TRUE)

########plotting map data################

#underweight children
ggplot() +
  geom_sf(data = merged_childmap_underweight, aes(geometry = geom, fill = undernut_percentages))+
  scale_fill_gradient(name = "Percentage of  Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of underweight Children by States") +
  theme_minimal()


<<<<<<< HEAD
#stunted children
=======
##This code below was me trying to work around the NA values, still working on it
##merged_childmap_data1 <- merged_childmap_data  merged_childmap_data[complete.cases(merged_childmap_data$underweight_class),]


#I don't think the map is quite right, below I'm trying to create a new data frame with a percentage
#column with only the underweight and NA value

?subset
map_merged_childmap <- subset(merged_childmap_data, underweight_class != "normal" | is.na(underweight_class))

>>>>>>> 53f4814 (latest on hinata, push from kelechi)
ggplot() +
  geom_sf(data = merged_childmap_stunting, aes(geometry = geom, fill = stunting_percentages))+
  scale_fill_gradient(name = "Percentage of stunted Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of stunted Children by States") +
  theme_minimal()

<<<<<<< HEAD
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
=======
#Starting to handle missing data
sum(is.na(merged_childmap_data$underweight_class))

#Trying the add state initials to the the states in the map
## your_dataset$new_variable <- substr(your_dataset$existing_variable, 1, 2)
>>>>>>> 53f4814 (latest on hinata, push from kelechi)
