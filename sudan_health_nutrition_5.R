# CMAM programme responsiveness ------------------------------------------------

#Create Time Column
cmam$time = match(cmam$Month,month.abb + (cmam$Year-2016)*12)

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
  time = integer(),
  CureRate = numeric(),
  DefaultRate = numeric(),
  DeathRate = numeric(),
  NonResponderRate = numeric(),
  AdmittedRate = numeric()
)

#Filling the Data Frame
for (i in 1:max(cmam$time)) {
  aux <- filter(cmam,time==i)
  
  # Calculate each indicator
  cureRate = sum(aux$Cured, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  defaultRate = sum(aux$Default, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  deathRate = sum(aux$Death, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  nonResponderRate = sum(aux$Non.Responder, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
  if(sum(aux$Screening, na.rm = TRUE)!=0)
    admittedRate = sum(aux$New.Admissions, na.rm = TRUE) / sum(aux$Screening, na.rm = TRUE)
  else 
    admittedRate = NA
  
  IndicatorsbyTime <- rbind(IndicatorsbyTime, c(i, cureRate, defaultRate, deathRate, nonResponderRate, admittedRate))
}
#Changing the name of columns
names(IndicatorsbyTime) <- c("Time", "CureRate", "DefaultRate", "DeathRate", "NonResponderRate", "AdmittedRate")

#Changing from short to long for plotting
IndicatorsLong <- pivot_longer(IndicatorsbyTime, 
                               cols = -Time, 
                               names_to = "RateType", 
                               values_to = "Value")
#Plotting
ggplot(IndicatorsLong, aes(x = Time, y = Value, color = RateType)) +
  geom_line() +
  geom_point() + 
  theme_minimal() + 
  labs(title = "Rates for all States by Time",
       x = "Time in month since Jan 2016",
       y = "Rate Value",
       color = "Rate Type") + 
  scale_color_brewer(palette = "Set1")


#New Mariano Part

States = unique(cmam$State)
IndicatorsbyTimeAndState <- data.frame(
  time = integer(),
  CureRate = numeric(),
  DefaultRate = numeric(),
  DeathRate = numeric(),
  NonResponderRate = numeric(),
  AdmittedRate = numeric(),
  State=character()
)

for (i in 1:max(cmam$time)) {
  for(j in States){
    aux <- filter(cmam,time==i & State==j)

    
    # Calculate each indicator
    cureRate = sum(aux$Cured, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
    defaultRate = sum(aux$Default, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
    deathRate = sum(aux$Death, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
    nonResponderRate = sum(aux$Non.Responder, na.rm = TRUE) / sum(aux$Total.Discharge, na.rm = TRUE)
    if(sum(aux$Screening, na.rm = TRUE)!=0)
      admittedRate = sum(aux$New.Admissions, na.rm = TRUE) / sum(aux$Screening, na.rm = TRUE)
    else 
      admittedRate = NA
  
    IndicatorsbyTimeAndState <- rbind(IndicatorsbyTimeAndState, c(i, cureRate, defaultRate, deathRate, nonResponderRate, admittedRate,j))

    }
}
names(IndicatorsbyTimeAndState) <- c("Time", "CureRate", "DefaultRate", "DeathRate", "NonResponderRate", "AdmittedRate", "State")

#Changing from short to long for plotting
IndicatorsLong2 <- pivot_longer(IndicatorsbyTimeAndState, 
                               cols = c(-Time, -State), 
                               names_to = "RateType", 
                               values_to = "Value")


p=IndicatorsLong2 %>%
  filter(RateType == "CureRate") %>%
  mutate(Time = as.numeric(as.character(Time)), Value = as.numeric(Value)) %>%
  arrange(State, Time) %>%
  ggplot(aes(x = Time, y = Value, group = State, color = State)) +  # Añadir 'group = State' aquí
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Cure Rates by State",
       x = "Time in month since Jan 2016",
       y = "Cure Rate",
       color = "State")

# Convert ggplot object to a plotly object
p_interactive <- ggplotly(p)

# Open in Viewer Pane (in RStudio) or in the default web browser
p_interactive



#Jojo Part
#Create the charts by year


print("hello i am jojo")


#Nei Part





#Prateek Part
#better visualization of all the rates at same time
#Make data frame and chart with rates related to cure rate
# use log scale
#or alternative

print("I am Prateek")


#Bok Part
print ("I am Bok Chol")

cmam_routine_data <- read.csv(file=
                                "https://raw.githubusercontent.com/OxfordIHTM/ihtm-hackathon-2024/main/data/cmam_routine_data.csv",
                              sep=",",header=TRUE)

##Calculation of cure rate per state ----
#Gazera state-----

gazera <- cmam_routine_data %>% filter(State == 'Gazera')

TotalNAd_Gazera <-sum(gazera$New.Admissions)
totalcured_gazera <- sum(gazera$Cured)
total_disch_gazera <- sum(gazera$Total.Discharge)
death_gazera <- sum(gazera$Death)

cureRate_Gazera <- totalcured_gazera/total_disch_gazera
deathRate_gazera <- death_gazera/total_disch_gazera


##Blue nile state-----
Blue_Nile <- cmam_routine_data %>% filter(State=='Blue Nile',preserve=TRUE)

totalNAd_Blue_Nile <- sum(Blue_Nile$New.Admissions)
total_cured_blue_nile <- sum(Blue_Nile$Cured)
disc_blue_nile <- sum(Blue_Nile$Total.Discharge)
death_blue_nile <- sum(Blue_Nile$Death)

cure_rate_blue_nile <- total_cured_blue_nile/disc_blue_nile

#end of code
