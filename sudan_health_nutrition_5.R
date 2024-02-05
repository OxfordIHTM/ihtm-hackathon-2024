#total
#new admissions
totalNAd=sum(cmam$New.Admissions)
#discharge
totalDisch=sum(cmam$Total.Discharge)

#total cured
totalCured=sum(cmam$Cured)
totalCureRate=totalCured/totalDisch

#total defaulter
totalDefault=sum(cmam$Default)
totalDefaultRate=totalDefault/totalDisch
