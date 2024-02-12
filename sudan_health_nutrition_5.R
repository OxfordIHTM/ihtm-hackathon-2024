library(tidyverse)
library(ggplot2)
library(dplyr)

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


