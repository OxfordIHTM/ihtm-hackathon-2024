<<<<<<< HEAD
source("packages.R")
#Load data
cmam <- read.csv("data/cmam_routine_data.csv")


#Create Time Column
cmam$time = match(cmam$Month,month.abb) + (cmam$Year-2016)*12

#Calculate Total Indicators in Data Frame
=======
cmam <- read.csv("data/cmam_routine_data.csv")

cmam$time = match(cmam$Month,month.abb) + (cmam$Year-2016)*12

>>>>>>> 2ac63f2 (Added Analysis of Indicators for all States together)
IndicatorsTotal = data.frame(
  Names = c("Cure Rate", "Default Rate" , "Death Rate" , "Non Responder Rate", "Admitted Rate"),
  Values = c(sum(cmam$Cured)/sum(cmam$Total.Discharge),
             sum(cmam$Default)/sum(cmam$Total.Discharge),
             sum(cmam$Death)/sum(cmam$Total.Discharge),
             sum(cmam$Non.Responder)/sum(cmam$Total.Discharge),
             sum(cmam$New.Admissions)/sum(cmam$Screening,na.rm=TRUE)
  )
)
<<<<<<< HEAD
#Show Global Indicators
IndicatorsTotal

#Create Empty Indicators by time Data Frame
IndicatorsbyTime <- data.frame(
  time = integer(),
=======
IndicatorsTotal

# Not working. Trying different thing

# IndicatorsbyTime = data.frame(
#   Names = c("Time", "Cure Rate", "Default Rate" , "Death Rate" , "Non Responder Rate", "Admitted Rate"),
#   for (i in 1:max(cmam$time)){
#     aux=filter(cmam, time ==i)
#     Values = c(i, 
#                sum(aux$Cured)/sum(aux$Total.Discharge),
#                sum(aux$Default)/aux(aux$Total.Discharge),
#                sum(aux$Death)/sum(aux$Total.Discharge),
#                sum(aux$Non.Responder)/sum(aux$Total.Discharge),
#                sum(aux$New.Admissions)/sum(aux$Screening,na.rm=TRUE))
#   }
#     
#   
# )


IndicatorsbyTime <- data.frame(
  Time = integer(),
>>>>>>> 2ac63f2 (Added Analysis of Indicators for all States together)
  CureRate = numeric(),
  DefaultRate = numeric(),
  DeathRate = numeric(),
  NonResponderRate = numeric(),
  AdmittedRate = numeric()
)

<<<<<<< HEAD
#Filling the Data Frame
for (i in 1:max(cmam$time)) {
  aux <- filter(cmam,time==i)
=======
for (i in 1:max(cmam$time)) {
  aux <- filter(cmam, time == i)
>>>>>>> 2ac63f2 (Added Analysis of Indicators for all States together)
  
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
<<<<<<< HEAD
#Changing the name of columns
names(IndicatorsbyTime) <- c("Time", "CureRate", "DefaultRate", "DeathRate", "NonResponderRate", "AdmittedRate")


#Changing from short to long for plotting
=======
names(IndicatorsbyTime) <- c("Time", "CureRate", "DefaultRate", "DeathRate", "NonResponderRate", "AdmittedRate")



>>>>>>> 2ac63f2 (Added Analysis of Indicators for all States together)
IndicatorsLong <- pivot_longer(IndicatorsbyTime, 
                               cols = -Time, 
                               names_to = "RateType", 
                               values_to = "Value")
<<<<<<< HEAD
#Plotting
=======

>>>>>>> 2ac63f2 (Added Analysis of Indicators for all States together)
ggplot(IndicatorsLong, aes(x = Time, y = Value, color = RateType)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Rates for all States by Time",
       x = "Time in month since Jan 2016",
       y = "Rate Value",
       color = "Rate Type") + 
  scale_color_brewer(palette = "Set1")
<<<<<<< HEAD


#Jojo Part
print("hello i am jojo")


#Nei Part
=======
>>>>>>> 2ac63f2 (Added Analysis of Indicators for all States together)





#Prateek Part
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