source("packages.R")
#Load data
cmam <- read.csv("data/cmam_routine_data.csv")


#Create Time Column
cmam$time = match(cmam$Month,month.abb) + (cmam$Year-2016)*12

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




#Jojo Part
summary(cmam)
mean(aux$Beginning.Of.Month)

# aux is the auxilliary for cmam. coz we don't want to change the dataset assigned to the group
# I want to do an interrupted time series analysis of the CMAM data
#Creating a binary variable called "intervention" indication pre and post CMAM. The intervention begun in month 13

# source("packages.R")
#Load data
cmam <- read.csv("data/cmam_routine_data.csv")


#Create Time Column
cmam$time = match(cmam$Month,month.abb) + (cmam$Year-2016)*12

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





#Prateek Part



#Bok Part







