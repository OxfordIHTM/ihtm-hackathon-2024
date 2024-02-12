library(ggplot2)
library(RColorBrewer)


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



##ggplot of maternal anc4 and state

