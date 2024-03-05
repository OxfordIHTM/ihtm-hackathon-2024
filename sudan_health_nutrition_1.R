#sudan maternal health bottleneck
library(ggplot2)
library(RColorBrewer)
library(dplyr)


maternal_health<-read.table("data/maternal_health.cvs",header = T, sep = ",")

##mean gestational age at first ANC visit
median_gest_age<-median(maternal_health$ancGestAge,na.rm = T)

##median gestational age at attendance to ANC at last pregnancy in 3 months


##testing the normality of our data----
hist(maternal_health$visitsANC)

##mean attendance to ANC----
median_attendance<- median(maternal_health$visitsANC,na.rm = T)
##overall median ANC attendance of mother is 3

##number of mothers who attend ANC at least 4 times------------
anc4<-sum(maternal_health$anc4=="1")
percent_anc<-(anc4/93424)*100

##any IFA
anyIFA<-sum(maternal_health$anyIFA=="1")
percent_anyIFA<-(anyIFA/93424)*100


unique(maternal_health$state_name)
##calculate these statistics for each state--------------
calculate_statistic <-function(maternal_health){
  
  stats_by_state<-maternal_health %>% 
    group_by(state_name) %>% 
  summarise(
    median_gest_age= median(ancGestAge, na.rm=TRUE),
    median_attendance=median(visitsANC, na.rm=TRUE),
    percent_anc4= sum(anc4=="1",na.rm = TRUE)/n()*100,    
    percent_anyIFA= sum(anyIFA=="1",na.rm = TRUE)/n()*100, 
    percent_IFA90= sum(ifa90=="1",na.rm = TRUE)/n()*100, 
    percent_tetanus= sum(tetanusDoses=="1",na.rm = TRUE)/n()*100    
  )
  return(stats_by_state)
 
}
stats_by_state<-calculate_statistic(maternal_health)

select(state_name,locality_name)

##group by state

mutate



##ggplot of maternal anc4 and state

