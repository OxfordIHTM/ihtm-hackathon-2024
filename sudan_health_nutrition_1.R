# Bottl4necks to maternal health sercies ---------------------------------------

## median gestational age at first ANC visit
median_gest_age<- median(maternal$ancGestAge, na.rm = T)
##median gestational age at  attendance to ANC at last pregnancy is 3 months

##testing the normality of our data-----------------------------------
hist(maternal$visitsANC)

##mean attendance to ANC------------------------------------------
median_attendance <- median(maternal$visitsANC, na.rm = T)
##overall median number of ANC attendance of mothers is 3 

##number of mothers who attend ANC at least 4 times---------------
anc4 <- sum(maternal$anc4== "1")
percent_anc4 <- (anc4/ 93424) * 100
## 41,116 women attended ANC at least 4 times, represents 44% of eligible women

## any IFA--------------------------------------------
anyIFA <- sum(maternal$anyIFA== "1")
percent_anyIFA <- (anyIFA/ 93424) * 100
## 63,097 women received at least one dose of IFA, representing 67.538%