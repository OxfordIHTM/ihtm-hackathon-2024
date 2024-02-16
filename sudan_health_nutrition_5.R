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

intervention<-13
set.seed(42)

my_zoo_series$Y <- round(IndicatorsbyTime$CureRate,1)

my_zoo_series$Y <- round(dataTS$Y,1)

d.temp <- rbind( head(my_zoo_series), 
                 my_zoo_series[ 13:24, ], 
                  
                 my_zoo_series[ 37:48, ],  
                 
                 tail(my_zoo_series) )

row.names( d.temp ) <- NULL  
pander( d.temp ) 

# Define the start date
start_date <- as.Date("2016-01-01")

# Create a sequence of dates for 48 months
date_sequence <- seq.Date(from = start_date, by = "quarter", length.out = 16)

# Display the first few dates to verify
head(date_sequence)

my_zoo_series$dates <- seq(start_date, by = "month", length.out = 48)

# Starting date
start_date <- as.Date("2016-01-01")

# Create a sequence of monthly dates for 48 months
dates <- seq(start_date, by = "month", length.out = 48)

# Generate example data for these 48 months
# Here, we're just creating a simple sequence as an example
data <- 1:48

# Create the zoo object
my_zoo_series <- zoo(data, order.by = dates)

# Display the first few entries of the zoo object
head(my_zoo_series)

my_zoo_series$datesss <-date_sequence

my_zoo_series <-IndicatorsbyTime

plot(my_zoo_series$dates, my_zoo_series$Y, type="l",
      bty="n", pch=19, col="black",
      xlim=c(0,20),ylim = c(0,1),
      xlab = "Time (datesss)", 
      ylab = "Cure_Rate" )   

ggplot(my_zoo_series,aes(dates, Y))+geom_line()+ylim(0.5,1.0)
      
summary(my_zoo_series)



#Nei Part





#Prateek Part



#Bok Part







