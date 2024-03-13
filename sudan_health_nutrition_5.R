# CMAM programme responsiveness ------------------------------------------------
source("packages.R")
#Load data
cmam <- read.csv("data/cmam_routine_data.csv")

#Create Time Column
Sys.setlocale("LC_TIME", "C")
cmam$time = match(cmam$Month,month.abb) + (cmam$Year-2016)*12
cmam$Date <- as.Date(paste(cmam$Year, cmam$Month, "01", sep="-"), format="%Y-%b-%d")
cmam



#Calculate Total Indicators in Data Frame
IndicatorsTotal = data.frame(
  Names = c("Cure Rate", "Default Rate" , "Death Rate" , "Non Responder Rate", "Admitted Rate"),
  Values = c(sum(cmam$Cured)/sum(cmam$Total.Discharge),
             sum(cmam$Default)/sum(cmam$Total.Discharge),
             sum(cmam$Death)/sum(cmam$Total.Discharge),
             sum(cmam$Non.Responder)/sum(cmam$Total.Discharge),
             sum(cmam$New.Admissions)/sum(cmam$Screening,na.rm=TRUE)
  )
)
#Show Global Indicators
IndicatorsTotal

#Create Empty Indicators by time Data Frame
IndicatorsbyTime <- data.frame(
  Date = as.Date(character()),
  CureRate = numeric(),
  DefaultRate = numeric(),
  DeathRate = numeric(),
  NonResponderRate = numeric(),
  AdmittedRate = numeric()
)


#Filling the Data Frame
for (date in unique(cmam$Date)) {
  aux <- filter(cmam, Date == date)
  
  # Calculate each indicator
  cureRate = sum(aux$Cured, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  defaultRate = sum(aux$Default, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  deathRate = sum(aux$Death, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  nonResponderRate = sum(aux$Non.Responder, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  if(sum(aux$Screening, na.rm = TRUE)!=0)
    admittedRate = sum(aux$New.Admissions, na.rm = TRUE) / sum(aux$Screening, na.rm = TRUE)
  else 
    admittedRate = NA
  
  
  newRow <- data.frame(Date = as.Date(date), CureRate = cureRate, DefaultRate = defaultRate, DeathRate = deathRate, NonResponderRate = nonResponderRate, AdmittedRate = admittedRate)
  IndicatorsbyTime <- rbind(IndicatorsbyTime, newRow)
}
#Changing the name of columns
names(IndicatorsbyTime) <- c("Date", "CureRate", "DefaultRate", "DeathRate", "NonResponderRate", "AdmittedRate")

#Changing from short to long for plotting
IndicatorsLong <- pivot_longer(IndicatorsbyTime, 
                               cols = -Date, 
                               names_to = "RateType", 
                               values_to = "Value")
#Plotting
ggplot(IndicatorsLong, aes(x = Date, y = Value, color = RateType)) +
  geom_line() +
  geom_point() + 
  theme_minimal() + 
  labs(title = "Rates for all States by Date",
       x = "Date",
       y = "Rate Value",
       color = "Rate Type") + 
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#New Mariano Part--------------

States = unique(cmam$State)
IndicatorsbyTimeAndState <- data.frame(
  Date = as.Date(character()),
  CureRate = numeric(),
  DefaultRate = numeric(),
  DeathRate = numeric(),
  NonResponderRate = numeric(),
  AdmittedRate = numeric(),
  State=character()
)

for (date in unique(cmam$Date)) {
  for (state in States) {
    aux <- filter(cmam, Date == date & State == state)
    
    
    cureRate = ifelse(sum(aux$Total.Discharge, na.rm = TRUE) > 0, sum(aux$Cured, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE), NA)
    defaultRate = ifelse(sum(aux$Total.Discharge, na.rm = TRUE) > 0, sum(aux$Default, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE), NA)
    deathRate = ifelse(sum(aux$Total.Discharge, na.rm = TRUE) > 0, sum(aux$Death, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE), NA)
    nonResponderRate = ifelse(sum(aux$Total.Discharge, na.rm = TRUE) > 0, sum(aux$Non.Responder, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE), NA)
    admittedRate = ifelse(sum(aux$Screening, na.rm = TRUE) > 0, sum(aux$New.Admissions, na.rm = TRUE) / sum(aux$Screening, na.rm = TRUE), NA)

    newRow <- data.frame(Date = as.Date(date), CureRate = cureRate, DefaultRate = defaultRate, DeathRate = deathRate, NonResponderRate = nonResponderRate, AdmittedRate = admittedRate, State = state)
    IndicatorsbyTimeAndState <- rbind(IndicatorsbyTimeAndState, newRow)
  }
}
names(IndicatorsbyTimeAndState) <- c("Date", "CureRate", "DefaultRate", "DeathRate", "NonResponderRate", "AdmittedRate", "State")

#Changing from short to long for plotting
IndicatorsLong2 <- pivot_longer(IndicatorsbyTimeAndState, 
                                cols = c(-Date, -State), 
                                names_to = "RateType", 
                                values_to = "Value")


#plot
IndicatorsLong2Filtered <- IndicatorsLong2 %>%
  filter(RateType == "CureRate") %>%
  arrange(State, as.Date(Date))


p <- ggplot(IndicatorsLong2Filtered, aes(x = Date, y = Value, group = State, color = State)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Cure Rates by State Over Time",
       x = "Date",
       y = "Cure Rate",
       color = "State") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#71, tabla includes indicators but state and date. I order as required
IndicatorsbyTimeAndState <- IndicatorsbyTimeAndState %>%
  dplyr::select(State, Date, CureRate, DefaultRate, DeathRate, NonResponderRate, AdmittedRate)
head(IndicatorsbyTimeAndState)

# Convert ggplot object to a plotly object
p_interactive <- ggplotly(p)

# Open in Viewer Pane (in RStudio) or in the default web browser
p_interactive


## Indicator by year:


IndicatorsByYear <- cmam %>%
  group_by(Year) %>%
  summarise(
    admissions = sum(New.Admissions, na.rm = TRUE),
    defaulters = sum(Default, na.rm = TRUE),
    cured = sum(Cured, na.rm = TRUE),
    death = sum(Death, na.rm = TRUE),
    non_responder = sum(Non.Responder, na.rm = TRUE),
    total_discharge = sum(Total.Discharge, na.rm = TRUE)
  ) %>%
  mutate(
    CureRate = cured / total_discharge,
    DeathRate = death / total_discharge,
    DefaultRate = defaulters / total_discharge,
    NonResponderRate = non_responder / total_discharge
  ) %>%
  dplyr::select(Year, admissions, defaulters, CureRate, DeathRate, DefaultRate, NonResponderRate)

# Rrsult
print(IndicatorsByYear)


IndicatorsLong3 <- pivot_longer(IndicatorsByYear, 
                               cols = -Year, 
                               names_to = "RateType", 
                               values_to = "Value") %>%
  filter(RateType != "admissions", RateType != "defaulters")

ggplot(IndicatorsLong3, aes(x = as.factor(Year), y = Value, fill = RateType)) +
  geom_bar(stat = "identity") +
  labs(title = "National CMAM performance",
       subtitle = "Sudan 2016-2019",
       x = "Year",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("CureRate" = "green", "DefaultRate" = "red", 
                    "DeathRate" = "black", "NonResponderRate" = "grey")) 



#73
ggplot(IndicatorsByYear, aes(x = Year, y = admissions)) +
  geom_line(colour="blue") +
  labs(title = "Admissions by Year",
       x = "Year",
       y = "Admissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#74
ggplot(IndicatorsByYear, aes(x = Year, y = defaulters)) +
  geom_line(colour="red") +
  labs(title = "Defaulters by Year",
       x = "Year",
       y = "Number of Defaulters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Jojo Part
#Create the charts by year


#create column for quarters
cmam <- cmam %>% mutate(Quarter = ceiling(as.numeric(cmam$time) / 3))


#Calculate Cure rate total in Data Frame
Cure_Rate_Total = data.frame(
  Names = c("Cure Rate"),
  Values = c(sum(cmam$Cured)/sum(cmam$Total.Discharge)
  )
)

#Create Empty Indicators by a Data Frame using quarters
Cure_Rate_by_Quarter <- data.frame(
  Quarter = integer(),
  Cure_Rate = numeric()
)

#Filling the Data Frame
for (i in 1:max(cmam$Quarter)) {
  aux <- filter(cmam,Quarter==i)
  
  # Calculate each indicator
  Cure_Rate = sum(aux$Cured, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  
  Cure_Rate_by_Quarter <- rbind(Cure_Rate_by_Quarter, c(i, Cure_Rate))
}
#Changing the name of columns
names(Cure_Rate_by_Quarter) <- c("Quarter", "Cure_Rate")

#Changing from short to long for plotting
IndicatorsLong <- pivot_longer(Cure_Rate_by_Quarter, 
                               cols = -Quarter, 
                               names_to = "RateType", 
                               values_to = "Value")
#Plotting
ggplot(IndicatorsLong, aes(x = Quarter, y = Value, color = RateType)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Cure Rate as a proxy for responsiveness of CMAM",
       # x = "Time in quarters since Jan 2016",
       y = "Cure Rate",
       color = "Rate Type") + 
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = c(4, 8, 12, 16), labels = c("Dec-16", "Dec-17", "Dec-18", "Dec-19" ))
# scale_x_discrete(labels=c('Dec-16', 'Dec-17', 'Dec-18', 'Dec-19'))



#Nei Part

#my charts are pritier
ggplot(IndicatorsLong3, aes(x = as.factor(Year), y = Value, fill = RateType)) +
  geom_bar(stat = "identity") +
  labs(title = "National CMAM performance - Prettier by Nei",
       subtitle = "Sudan 2016-2019",
       x = "Year",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("CureRate" = "#40E0D0", "DefaultRate" = "#FF5733", 
                               "DeathRate" = "black", "NonResponderRate" = "grey")) 


#Prateek Part
print("I am Prateek")
#My charts are with pastel colors <3
ggplot(IndicatorsLong3, aes(x = as.factor(Year), y = Value, fill = RateType)) +
  geom_bar(stat = "identity") +
  labs(title = "National CMAM performance - Pastel colors by Prateek",
       subtitle = "Sudan 2016-2019",
       x = "Year",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_fill_manual(values = c("CureRate" = "#96ceb4", "DefaultRate" = "#d581a2", 
                               "DeathRate" = "#485061", "NonResponderRate" = "#f4e7d0"))

#Bok Part
print ("I am Bok Chol")

##Calculation of cure rate per state ----
#Gazera state-----

gazera <- cmam %>% filter(State == 'Gazera')

TotalNAd_Gazera <-sum(gazera$New.Admissions)
totalcured_gazera <- sum(gazera$Cured)
total_disch_gazera <- sum(gazera$Total.Discharge)
death_gazera <- sum(gazera$Death)

cureRate_Gazera <- totalcured_gazera/total_disch_gazera
deathRate_gazera <- death_gazera/total_disch_gazera


##Blue nile state-----
Blue_Nile <- cmam %>% filter(State=='Blue Nile',preserve=TRUE)

totalNAd_Blue_Nile <- sum(Blue_Nile$New.Admissions)
total_cured_blue_nile <- sum(Blue_Nile$Cured)
disc_blue_nile <- sum(Blue_Nile$Total.Discharge)
death_blue_nile <- sum(Blue_Nile$Death)

cure_rate_blue_nile <- total_cured_blue_nile/disc_blue_nile

#end of code