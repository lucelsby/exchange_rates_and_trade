
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
               data.table,
               stringr)


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
  "gs_impo",
  "gs_exp",
  "gs_imp_cvm",
  "gs_exp_cvm",
  
  # purely goods data
  "g_imp",
  "g_exp",
  "g_imp_cvm",
  "g_exp_cvm"

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




## Download OECD Data ----------------------------------------------------------
# use the rdbnomics package to retrieve data from the OECD on members' industrial production
# this is used in the modelling as a proxy for global demand

# collect time series using rdbnomics
oecd_industrial_production_raw <- rdb(ids = "OECD/KEI/PRINTO01.OECD.ST.M")


# clean
oecd_industrial_production_clean <- oecd_industrial_production_raw %>%
  
  # collect only relevant columns
  dplyr::select(period, value) %>%
  
  # rename
  dplyr::rename(., ind_prod = value)




## Upload oil production data from input files ---------------------------------
# two bits of oil-related data are collected from the US Energy Information Administration website
# global oil production and US oil price
# these have both been downloaded and stored in the input_data file

# upload from raw csv file
eia_oil_production_raw <- as.data.frame(t(janitor::clean_names(read.csv("input_data/20221126_eia_oil_production.csv", skip = 1)))) %>%
  
  # remove the first column
  dplyr::select(-1) %>%
  
  
  # remove the empty row
  dplyr::slice(-c(1:2))


# clean
# create a function to extract the date from the short form in the excel file
extract_date <- function(x) {
  
  # define month numbers to map to names
  Months <- 1:12
  names(Months) <- month.abb
  
  # if the year number is less than 50, start the date with 2000 otherwise start with 1900
  ifelse(as.numeric(substr(x, 5, 6)) < 50, 
         paste0("20", substr(x, 5, 6), "-", 
                as.character(str_pad(unname(Months[str_to_title(substr(x, 1, 3))]), 2, pad = "0")), "-01"),
         paste0("19", substr(x, 5, 6), "-", 
                as.character(str_pad(unname(Months[str_to_title(substr(x, 1, 3))]), 2, pad = "0")), "-01"))
  
  
}

# extract the period from the raw data frame
period <- extract_date(rownames(eia_oil_production_raw))

# exrtact the time series of global oil production
oil_prod <- eia_oil_production_raw$V2

# combine into clean data frame
eia_oil_production_clean <- as.data.frame(cbind(period, oil_prod))

# convert character to date format
eia_oil_production_clean$period <- as.Date(eia_oil_production_clean$period)

  

