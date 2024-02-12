maternal <- read.table("data/maternal_health.csv", header = T, sep = ",")

##median attendance to ANC
median_attendance <- median(maternal$visitsANC, na.rm = T)
##median attendance=3

##number of mothers who attend ANC at least 4 times
Anc4 <- sum(maternal$anc4=="1")
percent_Anc4 <-(Anc4/93424)*100
## 41116 women attended ANC at least 4 times, representing 44% 

##Any IFA
AnyIFA <- sum(maternal$anyIFA=="1")
percent_AnyIFA <-(AnyIFA/93424)*100
##63097 women received at least one dose of IFA, representing 67.53% 

##Testing normality of our data
hist(maternal$visitsANC)