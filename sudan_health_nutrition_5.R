# read the data.
#in so doing, the cmam object can been assigned to values in the Environment

cmam <- read.csv("data/cmam_routine_data.csv")

#the following are the indicators we are interested in to assess reponsiveness of the CMAM project:

# the responsiveness indicators are 5
#cure rate (cured/discharges)
#defaulter rate (defaulters/discharged)
#death rate (dead/discharged)
#non-response rate (nonresponders/discharged)
#admission rate (admitted/screened)

#the operational performance indicator is RUTF consumption per month

#adding a column to the dataframe for the calculation of each of these

cmam$curerate <- c(cmam$Cured/cmam$Total.Discharge)
cmam$defaultrate <- c(cmam$Default/cmam$Total.Discharge)
cmam$deathrate <- c(cmam$Death/cmam$Total.Discharge)
cmam$nonresponserate <-c(cmam$Non.Responder/cmam$Total.Discharge)
cmam$admissionrate <-c(cmam$New.Admissions/cmam$Screening)

# Assuming 'cmam' is my dataframe and 'State' is the name of the column of interest, number of States is:
num_classes <- length(unique(cmam$State))


