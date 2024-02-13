#######classifying children and mothers nutrition status#######


#Classifying underweight children

classify_underweight_child <- function(waz) {
  ifelse(
    waz <= -2  , "underweight",
    ifelse(
      waz <= -3, "severe underweight", "normal"
    )
  )
}

#Adding a new underweight status column to the child data frame

child$underweight_class <- classify_underweight_child(child$waz)

#classifying stunted children
classify_stunting_child <- function(haz) {
  ifelse(
    haz < -3, "severe stunting",
    ifelse(
      haz < -2, "stunting", "normal"
    )
  )
}

#Adding a new stunting status column to the child data frame
child$stunting_status <- classify_stunting_child(child$haz)

#classifying wasted children

classify_wasting_child <- function(whz, muac, oedema) {
  ifelse(whz < -2 | muac < 125 | oedema == 1, "wasting", "normal")
}

#Adding a new stunting status column to the child data frame
child$wasting <- classify_wasting_child(child$whz, child$muac, child$oedema)

#classifying undernourished mothers

classify_undernut_mother <- function(muac) {
  ifelse(
   muac < 220, "undernourished","normal")
}

# Create a new variable to classify mother's nutrition status based on MUAC
maternal$undernut <- classify_undernut_mother(maternal$muac)



#########calculating percentage malnourishment by state########



##starting by counting the values of underweight by state

child_map_underweight <- child %>% group_by(state_name,underweight_class,state_id) %>% count() %>% ungroup() 
child_map_stunting <- child %>% group_by(state_name,stunting_status,state_id) %>% count() %>% ungroup() 
child_map_wasting <- child %>% group_by(state_name,wasting,state_id) %>% count() %>% ungroup() 
maternal_map_undernourished <- maternal %>% group_by(state_name, undernut, state_id) %>% count() %>% ungroup()

##Getting a percentage from the previous count
get_perc = function(x){
  percentage <- round(x/sum(x)*100,1)
  percentage
}

child_map_underweight <- child_map_underweight %>% group_by(state_id) %>% mutate(undernut_percentages = (get_perc(n)))
child_map_stunting <- child_map_stunting %>% group_by(state_id) %>% mutate(stunting_percentages = (get_perc(n)))
child_map_wasting <- child_map_wasting %>% group_by(state_id) %>% mutate(wasting_percentages = (get_perc(n)))
maternal_map_undernourished <- maternal_map_undernourished %>% group_by(state_id) %>% mutate(undernut_percentages = get_perc(n))


########Retrieving map data#############

sudan_map_spec <- download_sudan_maps(download_url = "https://data.humdata.org/dataset/a66a4b6c-92de-4507-9546-aa1900474180/resource/e5ef3cc7-f105-4565-8d73-e08bb756f1c1/download/sdn_adm_cbs_nic_ssa_20200831.gdb.zip")

sudan0 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[1])
sudan1 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[2])
sudan2 <- st_read(dsn = sudan_map_spec$dsn, layer = sudan_map_spec$layers[4])

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