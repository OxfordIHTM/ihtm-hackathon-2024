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

child_map <- child %>% group_by(state_name,underweight_class) %>% count() %>% ungroup() 

##Getting a percentage from the previous count
get_perc = function(x){
  percentage <- round(x/sum(x)*100,1)
  percentage
}

child_map <- child_map %>% group_by(state_name) %>% mutate(percentages = (get_perc(n)))

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


##Retrieving map data

sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])

##merging map data with the child_map data frame

?merge.data.frame()
merged_childmap_data <- merge.data.frame(sudan1, child_map, by.x = "admin1Name_en", by.y = "state_name", all.x = TRUE)

#plotting map data
ggplot() +
  geom_sf(data = merged_childmap_data, aes(geometry = Shape, fill = percentages))+
  scale_fill_gradient(name = "Percentage of Undernourished Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of Undernourished Children by States") +
  theme_minimal()


##This code below was me trying to work around the NA values, still working on it
##merged_childmap_data1 <- merged_childmap_data  merged_childmap_data[complete.cases(merged_childmap_data$underweight_class),]


#I don't think the map is quite right, below I'm trying to create a new data frame with a percentage
#column with only the underweight and NA value

?subset
map_merged_childmap <- subset(merged_childmap_data, underweight_class != "normal" | is.na(underweight_class))

ggplot() +
 geom_sf(data = map_merged_childmap, aes(geometry = Shape, fill = percentages))+
  scale_fill_gradient(name = "Percentage of Undernourished Children", low = "lightblue", high = "darkblue", na.value = "gray50") +
  labs(title = "Percentage of Undernourished Children by States") +
  theme_minimal()

#Starting to handle missing data
sum(is.na(merged_childmap_data$underweight_class))

#Trying the add state initials to the the states in the map
## your_dataset$new_variable <- substr(your_dataset$existing_variable, 1, 2)
