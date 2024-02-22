
# Classifying underweight children

classify_underweight_child <- function(waz) {
  ifelse(
    waz <= -2  , "underweight",
    ifelse(
      waz <= -3, "severe underweight", "normal"
    )
  )
}


#classifying stunted children
classify_stunting_child <- function(haz) {
  ifelse(
    haz < -3, "severe stunting",
    ifelse(
      haz < -2, "stunting", "normal"
    )
  )
}


#classifying wasted children
classify_wasting_child <- function(whz, muac, oedema) {
  ifelse(whz < -2 | muac < 125 | oedema == 1, "wasting", "normal")
}


#classifying undernourished mothers
classify_undernut_mother <- function(muac) {
  ifelse(
    muac < 220, "undernourished","normal")
}

##Getting a percentage from the previous count
get_perc = function(x){
  percentage <- round(x/sum(x)*100,1)
  percentage
}

