#library(tidyverse)

#functions for classifying
#Underweight is defined as the children with weight-for-age Z-score (WAZ)
#<−2SD and severe underweight is defined as the children with WAZ <−3SD

classify_underweight_child <- function(waz) {
  ifelse(
    waz <= -2  , "underweight",
    ifelse(
      waz <= -3, "severe underweight", "normal"
    )
  )
}

classify_underweight_child(child$waz)
child$underweight_class <- classify_underweight_child(child$waz)

##grouping underweight classes by locality

child_map <- child %>% group_by(state_name,underweight_class) %>% count() %>% ungroup() 

get_perc = function(x){
  percentage <- round(x/sum(x)*100,1)
  percentage
}

child_map <- child_map %>% group_by(state_name) %>% mutate(percentages = (get_perc(n)))

