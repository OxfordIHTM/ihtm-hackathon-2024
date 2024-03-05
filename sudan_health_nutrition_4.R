#######classifying children and mothers nutrition status#######

#Adding a new underweight status column to the child data frame
child$underweight_class <- classify_underweight_child(child$waz)

#Adding a new stunting status column to the child data frame
child$stunting_status <- classify_stunting_child(child$haz)

#Adding a new stunting status column to the child data frame
child$wasting <- classify_wasting_child(child$whz, child$muac, child$oedema)

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

plot_underweight <- ggplot() +
  geom_sf(data = merged_childmap_underweight, aes(geometry = geom, fill = undernut_percentages)) +
  scale_fill_viridis_c(name = "Percentage of Children", option = "E", na.value = "gray50") +
  labs(title = "Percentage of Underweight Children by States") +
  theme_minimal()

# Add labels using geom_sf_text()
plot_underweight_with_labels <- plot_underweight +
  geom_sf_text(data = merged_childmap_underweight, aes(geometry = geom, label = state_name), size = 1)

# Arrange the map using patchwork
final_map <- plot_underweight_with_labels + plot_layout(ncol = 1)

# Display the final map
final_map


#stunted children
plot_stunted <- ggplot() +
  geom_sf(data = merged_childmap_stunting, aes(geometry = geom, fill = stunting_percentages)) +
  scale_fill_viridis_c(name = "Percentage of Children", option = "E", na.value = "gray50") +
  labs(title = "Percentage of Stunted Children by States") +
  theme_minimal()

# Add labels using geom_sf_text()
plot_stunting_with_labels <- plot_stunted +
  geom_sf_text(data = merged_childmap_stunting, aes(geometry = geom, label = state_name), size = 1)

# Arrange the map using patchwork
final_map <- plot_stunting_with_labels + plot_layout(ncol = 1)

# Display the final map
final_map

#Wasting children
plot_wasting <- ggplot() +
  geom_sf(data = merged_childmap_wasting, aes(geometry = geom, fill = wasting_percentages)) +
  scale_fill_viridis_c(name = "Percentage of Children", option = "E", na.value = "gray50") +
  labs(title = "Percentage of wasted Children by States") +
  theme_minimal()

# Add labels using geom_sf_text()
plot_wasting_with_labels <- plot_wasting +
  geom_sf_text(data = merged_childmap_wasting, aes(geometry = geom, label = state_name), size = 1)

# Arrange the map using patchwork
final_map <- plot_wasting_with_labels + plot_layout(ncol = 1)

# Display the final map
final_map

#Undernourished mothers
plot_undernourishedmothers <- ggplot() +
  geom_sf(data = merged_maternalmap_undernourished, aes(geometry = geom, fill = undernut_percentages)) +
  scale_fill_viridis_c(name = "Percentage of Children", option = "E", na.value = "gray50") +
  labs(title = "Percentage of Stunted Children by States") +
  theme_minimal()

# Add labels using geom_sf_text()
plot_undernourishedmothers_with_labels <- plot_undernourishedmothers +
  geom_sf_text(data = merged_maternalmap_undernourished, aes(geometry = geom, label = state_name), size = 1)

# Arrange the map using patchwork
final_map <- plot_undernourishedmothers_with_labels + plot_layout(ncol = 1)

# Display the final map
final_map


#### Plot faceted map for every classification ----
ggplot(data = , aes(fill = Observation.Value))+
  geom_sf()+
  scale_fill_viridis_c()+
  labs(title = "World Literacy Rates Over time") +
  facet_wrap(.~Time.Period, ncol = 5)+
  theme_minimal()
