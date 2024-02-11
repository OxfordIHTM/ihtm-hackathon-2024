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
cmam_routine_data
nrow(cmam_routine_data)
ncol(cmam_routine_data)
