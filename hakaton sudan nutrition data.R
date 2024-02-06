# Load R package dependencies --------------------------------------------------

## Load general use packages ----
library(openxlsx)     ## Read and write XLSX files
library(dplyr)        ## Data wrangling and manipulation using tidy approach
library(tidyr)        ## Data wrangling and manipulation using tidy approach
library(ggplot2)      ## Data visualisation using tidy approach
library(rmarkdown)    ## Literate programming and report generation
library(remotes)      ## For installing packages from GitHub
library(here)         ## Utility package for working directory helper


## Load packages for mapping ----
library(sf)           ## Main package for mapping

spatial_data<-st_read("data/child_health.csv")

classify_child_health <- function(haz) {
  ifelse(
    haz < -2, "stunted",
    ifelse(
      haz < -3, "severe stunting",
     
    )
  )
}
#use dplyer package
# group by the district and differentaion the gorup as undernutrion and normal nutrition
#use the n function to know how many are undernutritioned
#calculated the parcentage and creat a table with corresponding district
#use spatial distribution or heat areas in the map