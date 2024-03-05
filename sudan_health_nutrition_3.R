#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("pubh")
#install.packages("ggmosaic")
#install.packages("rstatix")
#install.packages("Publish")

library(tidyverse)
library(janitor)
library(pubh)
library(ggmosaic)
library(rstatix)
library(Publish)


# GENERAL CHARACTERISTICS 
library(gtsummary)
tbl_summary(child_health, include = c(accessEducation, accessBasicEducation, everAttendedSchool))

# INDIVIDUAL FACTORS 
tbl_summary(child_health, include = c(age, sex), by = accessEducation)
tbl_summary(child_health, include = c(age, sex), by = accessBasicEducation)
tbl_summary(child_health, include = c(age, sex), by = accessBasicEducation)

# HEALTH-RELATED FACTORS 
tbl_summary(child_health, include = c(vaccineRecord, diarrhoea), by = accessEducation)

