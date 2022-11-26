
### DATA COLLECTION ############################################################

# [Description of what this script does]



## Setup Environment ----------------------------------------------------------


# load pacman for package management
library(pacman)


# install and load relevant packages
pacman::p_load(dplyr,
               tidyverse,
               janitor,
               readxl,
               lubridate,
               rdbnomics,
               data.table)


# clear environment
rm(list = ls())
graphics.off()
options(scipen = 999)


## Download ONS Data -----------------------------------------------------------
# use the rdbnomics package to retrieve data from the ONS on UK trade flows


# create empty object to store the data
ons_data_raw <- NULL


# define the series ID's of the ONS trade data to collect
mret_ids <- c(
  
  ## Goods and Services Trade Data
  "IKBI", # UK imports of goods and services from the world - value measure
  "IKBH", # UK exports of goods and services to the world - value measure
  "IKBL", # UK imports of goods and services from the world - chain volume measure
  "IKBK",  # UK exports of goods and services to the world - chain volume measure
  
  ## Purely Goods Trade Data
  "BOKH", # UK imports of just goods from the world - value measure
  "BOKG", # UK exports of just goods to the world - value measure
  "BQKO", # UK imports of just goods from the world - chain volume measure
  "BQKQ" # UK exports of just goods to the world - chain volume measure

  )


# define a list of variable names to correspond to each series (must be in order)
mret_names <- c(
  ## goods and services data
  "gs_imports",
  "gs_exports",
  "gs_imports_cvm",
  "gs_exports_cvm",
  
  # purely goods data
  "g_imports",
  "g_exports",
  "g_imports_cvm",
  "g_exports_cvm"

  )


# Collect the monthly time series corresponding to each series ID 
# and bind into a single data frame
for (mret_id in mret_ids){
  
  ons_data_raw <- rbind(ons_data_raw,
                    
                    rdb(ids = paste0("ONS/MRET/", mret_id, ".M")))
  
}


# clean
ons_data_clean <- ons_data_raw %>%
  
  # collect only relevant columns
  dplyr::select(period, title, value) %>%
  
  # pivot to a wider data frame with each series as a seperate variable column
  pivot_wider(names_from = title, values_from = value) %>%
  
  # rename based on pre-defined variable names
  data.table::setnames(., old = c('period', mret_ids),
                       
                       new = c('period', mret_names)) %>%
  
  # Drop any rows with missing values
  drop_na()






