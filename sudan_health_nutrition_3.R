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

#Health-related factors 

child <- child %>%
  mutate(everAttendedSchool = factor(everAttendedSchool, labels = c("No", "Yes")), 
         safe_water = factor(vaccineRecord, labels = c("No", "Yes")))

child %>%
  drop_na(everAttendedSchool, vaccineRecord) %>%
  tabyl(everAttendedSchool, vaccineRecord) 

child %>%
    cross_tbl(child, by = "everAtendedSchool")





