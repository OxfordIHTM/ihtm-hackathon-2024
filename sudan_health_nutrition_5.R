library(tidyverse)
library(ggplot2)

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
