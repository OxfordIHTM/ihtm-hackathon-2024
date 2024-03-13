# Counting the childen that had BCG + 
BCG_positives <- sum(child$coverageBCG == 1, na.rm = TRUE)

# Print the result of the childen that had BCG + 
print(BCG_positives)

# Total number of rows (total children)
total_rows <- nrow(child)

# Calculate the percentage coverage for BCG
coverage_BCG <- (BCG_positives / total_rows) * 100
print(coverage_BCG)

# Bottlenecks to expanded programme on immunisation access ---------------------

# Total number of children that got BCG vaccine

BCG_positive <- sum(child$coverageBCG == 1, na.rm = TRUE)
print(BCG_positive)

# Total number of children eligible for BCG vaccine
BCG_eligible <- sum(child$coverageBCG %in% c(0, 1), na.rm = TRUE)
print(BCG_eligible)

# Calculate % BCG coverage in all children 

BCG_coverage <- (BCG_positive / BCG_eligible) * 100
print(BCG_coverage)


# Total number of eligible children that got Penta1 

Penta1_positive <- sum(child$coveragePenta1 == 1, na.rm = TRUE)
print(Penta1_positive)

# Total number of children eligible for Penta1 vaccine

Penta1_eligible <- sum(child$coveragePenta1 %in% c(0, 1), na.rm = TRUE)
print(Penta1_eligible)

# Calculate % Penta1 coverage in eligible children 

Penta1_coverage <- (Penta1_positive / Penta1_eligible) * 100
print(Penta1_coverage)


# Total number of eligible children that got Penta3 

Penta3_positive <- sum(child$coveragePenta3 == 1, na.rm = TRUE)
print(Penta3_positive)

# Total number of children eligible for Penta3 vaccine

Penta3_eligible <- sum(child$coveragePenta3 %in% c(0, 1), na.rm = TRUE)
print(Penta3_eligible)

# Calculate % Penta3 coverage in eligible children 

Penta3_coverage <- (Penta3_positive / Penta3_eligible) * 100
print(Penta3_coverage)


# Total number of eligible children that got OPV1 

OPV1_positive <- sum(child$coverageOPV1 == 1, na.rm = TRUE)
print(OPV1_positive)

# Total number of children eligible for OPV1 vaccine

OPV1_eligible <- sum(child$coverageOPV1 %in% c(0, 1), na.rm = TRUE)
print(OPV1_eligible)

# Calculate % OPV1 coverage in eligible children 

OPV1_coverage <- (OPV1_positive / OPV1_eligible) * 100
print(OPV1_coverage)


# Total number of eligible children that got OPV3 

OPV3_positive <- sum(child$coverageOPV3 == 1, na.rm = TRUE)
print(OPV3_positive)

# Total number of children eligible for OPV3 vaccine

OPV3_eligible <- sum(child$coverageOPV3 %in% c(0, 1), na.rm = TRUE)
print(OPV3_eligible)

# Calculate % OPV3 coverage in eligible children 

OPV3_coverage <- (OPV3_positive / OPV3_eligible) * 100
print(OPV3_coverage)

# Total number of eligible children that got Measles1 

Measles1_positive <- sum(child$coverageMeasles1 == 1, na.rm = TRUE)
print(Measles1_positive)

# Total number of children eligible for Measles1 vaccine

Measles1_eligible <- sum(child$coverageMeasles1 %in% c(0, 1), na.rm = TRUE)
print(Measles1_eligible)

# Calculate % Measles1 coverage in eligible children 

Measles1_coverage <- (Measles1_positive / Measles1_eligible) * 100
print(Measles1_coverage)

# Total number of eligible children that got Measles2 

Measles2_positive <- sum(child$coverageMeasles2 == 1, na.rm = TRUE)
print(Measles2_positive)

# Total number of children eligible for Measles2 vaccine

Measles2_eligible <- sum(child$coverageMeasles2 %in% c(0, 1), na.rm = TRUE)
print(Measles2_eligible)

# Calculate % Measles2 coverage in eligible children 

Measles2_coverage <- (Measles2_positive / Measles2_eligible) * 100
print(Measles2_coverage)

# Calculate number of children eligible for BCG per state

eligible_BCG_per_state <- child %>% filter(!is.na(coverageBCG)) %>% group_by(state_name) %>% summarise(Eligible = n())

print(eligible_BCG_per_state)

# Calculate number of children that received BCG per state
BCG_positive_per_state <- child %>% filter(coverageBCG == 1) %>% group_by(state_name) %>% summarise(BCG_positive = n())

print(BCG_positive_per_state)

# Calculate percentage BCG coverage per state

BCG_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  BCG_Eligible = sum(coverageBCG %in% c(0, 1), na.rm = TRUE),
  BCG_positive = sum(coverageBCG == 1, na.rm = TRUE),
  BCG_Coverage = (BCG_positive / BCG_Eligible) * 100)

print(BCG_coverage_per_state)

# Calculate national penta droupout rate

National_Penta_dropout <- (((Penta1_positive - Penta3_positive)/Penta1_positive)*100)
print(National_Penta_dropout)

# Calculate dropout rates between Penta1 and Penta3 for each state
State_penta_dropout <- child %>% group_by(state_name) %>% summarise(
  Penta1_positive = sum(coveragePenta1 == 1, na.rm = TRUE),
  Penta3_positive = sum(coveragePenta3 == 1, na.rm = TRUE),
  Dropout_Rate = ((Penta1_positive - Penta3_positive) / Penta1_positive) * 100
)

print(State_penta_dropout)

# Calculate coverage per state 

# Calculate percentage Penta1 coverage per state

Penta1_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  Penta1_Eligible = sum(coveragePenta1 %in% c(0, 1), na.rm = TRUE),
  Penta1_positive = sum(coveragePenta1 == 1, na.rm = TRUE),
  Penta1_Coverage = (Penta1_positive / Penta1_Eligible) * 100)

print(Penta1_coverage_per_state)

# Calculate percentage Penta3 coverage per state

Penta3_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  Penta3_Eligible = sum(coveragePenta3 %in% c(0, 1), na.rm = TRUE),
  Penta3_positive = sum(coveragePenta3 == 1, na.rm = TRUE),
  Penta3_Coverage = (Penta3_positive / Penta3_Eligible) * 100)

print(Penta3_coverage_per_state)

# Calculate percentage OPV1 coverage per state

OPV1_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  OPV1_Eligible = sum(coverageOPV1 %in% c(0, 1), na.rm = TRUE),
  OPV1_positive = sum(coverageOPV1 == 1, na.rm = TRUE),
  OPV1_Coverage = (OPV1_positive / OPV1_Eligible) * 100)

print(OPV1_coverage_per_state)

# Calculate percentage OPV3 coverage per state

OPV3_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  OPV3_Eligible = sum(coverageOPV3 %in% c(0, 1), na.rm = TRUE),
  OPV3_positive = sum(coverageOPV3 == 1, na.rm = TRUE),
  OPV3_Coverage = (OPV3_positive / OPV3_Eligible) * 100)

print(OPV3_coverage_per_state)

# Calculate percentage Measles1 coverage per state

Measles1_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  Measles1_Eligible = sum(coverageMeasles1 %in% c(0, 1), na.rm = TRUE),
  Measles1_positive = sum(coverageMeasles1 == 1, na.rm = TRUE),
  Measles1_Coverage = (Measles1_positive / Measles1_Eligible) * 100)

print(Measles1_coverage_per_state)

# Calculate percentage Measles2 coverage per state

Measles2_coverage_per_state <- child %>% group_by(state_name) %>% summarise(
  Measles2_Eligible = sum(coverageMeasles2 %in% c(0, 1), na.rm = TRUE),
  Measles2_positive = sum(coverageMeasles2 == 1, na.rm = TRUE),
  Measles2_Coverage = (Measles2_positive / Measles2_Eligible) * 100)

print(Measles2_coverage_per_state)

# Merging all the coverages per state

EPI_df <- left_join(Measles1_coverage_per_state, Measles2_coverage_per_state) %>%
  left_join(BCG_coverage_per_state) %>%
  left_join(OPV1_coverage_per_state) %>%
  left_join(OPV3_coverage_per_state) %>%
  left_join(Penta1_coverage_per_state) %>%
  left_join(Penta3_coverage_per_state) %>%
  dplyr::select("state_name", "Measles1_Coverage", "Measles2_Coverage", "BCG_Coverage", "OPV1_Coverage", "OPV3_Coverage", "Penta1_Coverage", "Penta3_Coverage")

print(EPI_df)

ggplot(EPI_df, aes(x = state_name, y = BCG_Coverage)) +
  geom_col(fill = "skyblue", width = 0.7) +
  labs(title = "BCG Positive Children per State", x = "State", y = "Number of Children")

#Each state is represented by a bar, and the counts of
#Penta1 positive- blue, Penta3 positive- green segments. 
#red line- drop out rate

ggplot(EPI_df, aes(x = state_name)) +
  geom_bar(aes(y = Penta1_Coverage), fill = "blue", alpha = 0.5, stat = "identity", position = "dodge") +
  geom_bar(aes(y = Penta3_Coverage), fill = "green", alpha = 0.5, stat = "identity", position = "dodge") +
  
  labs(title = "Penta1 and Penta3 Positive Counts with Dropout Rate",
       x = "State",
       y = "Count",
       color = "Dose") +
  theme_minimal()

