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
print("hello i am jojo")


#Nei Part





#Prateek Part
print("I am Prateek")


#Bok Part
print ("I am Bok Chol")







>>>>>>> e457e082089d5f870eb94fa6717656b3921447ab
