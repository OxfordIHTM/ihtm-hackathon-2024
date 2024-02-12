##gest age at first ANC visit
##no of ANC visits
##at least 4 ANC visits
##any IFA

##median gestational age at first ANC visit during last pregnancy
median_gest_age <- median(maternal$ancGestAge, na.rm = T)
##the median gestational age at first ANC during the last pregnancy is 3 months

##median number of ANC visits
median_ancvisits <- median(maternal$visitsANC, na.rm = T)
##the median number of ANC visits in eligible women during pregnancy is 3 times

##at least 4 ANC visits in line with the WHO prescribed standard of care
proportionanc4 <- sum(maternal$anc4 == "1")
print(proportionanc4)
propanc4 <- (proportionanc4/93424) * 100
print(propanc4)
##Out of 93,424 women, 41,116(44%) attended at least 4 ANC visits during pregnancy

##any IFA
proportionanyIFA <- sum(maternal$anyIFA == "1")
print(proportionanyIFA)
propanyIFA <- (proportionanyIFA/93424) * 100
print(propanyIFA)
##Out of 93,424 women, 63,097 (67.54%) received at least one dose of prenatal Iron and Folic acid