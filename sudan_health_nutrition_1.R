# Bottlenecks to maternal health sercies ---------------------------------------

## median gestational age at first ANC visit
median_gest_age<- median(maternal$ancGestAge, na.rm = T)
##median gestational age at  attendance to ANC at last pregnancy is 3 months

##testing the normality of our data-----------------------------------
hist(maternal$visitsANC)

##mean attendance to ANC------------------------------------------
median_attendance <- median(maternal$visitsANC, na.rm = T)
##overall median number of ANC attendance of mothers is 3 

##number of mothers who attend ANC at least 4 times---------------
anc4 <- sum(maternal$anc4== "1")
percent_anc4 <- (anc4/ 93424) * 100
## 41,116 women attended ANC at least 4 times, represents 44% of eligible women

## any IFA--------------------------------------------
anyIFA <- sum(maternal$anyIFA== "1")
percent_anyIFA <- (anyIFA/ 93424) * 100
## 63,097 women received at least one dose of IFA, representing 67.538%

unique(maternal$state_name)
##calculate these statistics for each state------------------------------
calculate_statistics <- function(maternal_data){
  
  stats_by_state <- maternal_data %>% 
    group_by(state_name) %>% 
    summarise(
      median_gest_age = median(ancGestAge, na.rm = TRUE),
      median_attendance = median(visitsANC, na.rm = TRUE),
      percent_anc4 = sum(anc4 == "1" , na.rm = TRUE)/n() * 100,
      percent_anyIFA = sum (anyIFA == "1", na.rm = TRUE)/n() * 100,
      percent_IFA90 = sum(ifa90 == "1", na.rm = TRUE)/n()* 100,
      percent_tetanus = sum (tetanusDoses == "1", na.rm = TRUE)/n() * 100
    )
  
  return(stats_by_state)
}


stats_by_state <- calculate_statistics(maternal)

# Define the data for each column as separate vectors
state_name <- c("Al-Gadarif", "Al-Gazeera", "Blue Nile", "Central Darfur", "East Darfur",
                "Kassala", "Khartoum", "North Darfur", "North Kourdofan", "Northern",
                "Red Sea", "River Nile", "Sinar", "South Darfur", "South Kourdofan",
                "West Darfur", "West Kourdofan", "White Nile")
median_gest_age <- c(3, 3, 3, 3, 4, 4, 3, 4, 3, 2, 5, 3, 3, 3, 4, 3, 4, 3)
median_attendance <- c(3, 4, 3, 0, 2, 3, 6, 3, 4, 5, 2, 3, 3, 3, 4, 3, 3, 3)
percent_anc4 <- c(35.9, 52.1, 39.2, 23.5, 33.8, 47.0, 77.7, 34.9, 55.7, 74.6,
                  28.6, 47.3, 46.4, 40.9, 54.3, 37.6, 35.5, 48.9)
percent_anyIFA <- c(72.2, 68.5, 64.2, 43.5, 54.8, 83.1, 84.5, 65.9, 80.6, 90.3,
                    70.7, 65.8, 77.3, 47.7, 70.5, 56.3, 65.4, 81.8)
percent_IFA90 <- c(44.9, 44.5, 36.6, 23.1, 12.0, 57.6, 72.3, 25.6, 48.3, 73.6,
                   47.0, 46.0, 49.3, 14.1, 28.4, 19.2, 19.8, 61.1)

# Create a data frame using the data
state_data <- data.frame(
  state = state_name,
  anc4 = percent_anc4,
  anyIFA = percent_anyIFA,
  IFA90 = percent_IFA90
)

##reshapetable
# Reshape the data into a long format
state_data_long <- state_data %>%
  pivot_longer(cols = c(anc4, anyIFA, IFA90), names_to = "Variable", values_to = "Value")

ggplot(stats_by_state, aes(x = state_name, y = percent_anc4, fill = state_name))+
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "ANC ATTENDANCE BY STATE",
       x= NULL,
       y = "% ANC ATTENDANCE (4 visits minimum)")

ggplot(stats_by_state, aes(x = state_name, y = percent_anyIFA))+
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "ANTENATAL SUPPLEMENTS PRESCRIBED BY STATE",
       x= NULL,
       y = "IFA (at least one dose)")


ggplot(stats_by_state, aes(x = state_name, y = percent_IFA90))+
  geom_col()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "3 MONTH IFA SUPPLEMENTATION PRESCRIBED BY STATE",
       x= NULL,
       y = "IFA (3 months)")

ggplot(state_data_long, aes(x = Variable, y = Value, fill = Variable)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Bar Plot for", state_name), x = "Variable", y = "Value")+
    facet_wrap(.~state, 6,3)+
  theme(legend.position = "none")+
  labs(x = NULL,
       y = "PERCENTAGE",
       title = "ANTENATAL SERVICE BOTTLENECKS BY STATE IN SUDAN")




