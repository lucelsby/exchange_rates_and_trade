
### TWO-VARIABLE VAR MODEL #####################################################

# [Description of what this script does]



## Setup Environment -----------------------------------------------------------


# load pacman for package management
library(pacman)


# install and load relevant packages
pacman::p_load(dplyr,
               tidyverse,
               janitor,
               readxl,
               lubridate,
               data.table,
               gridExtra,
               ggplot2,
               ggpubr,
               xts,
               tseries,
               fpp,
               vars,
               mFilter,
               TSstudio,
               forecast,
               tsbox,
               rdbnomics,
               stringr,
               fredr,
               ggpubr,
               purrr)


# clear environment
rm(list = ls())
graphics.off()
options(scipen = 999)


# source the functions R script to load pre-defined functions
source("0_functions.R")

# read in data
data <- read.csv("clean_data/data.csv")





## 2-Variable System -----------------------------------------------------------

# we start with a simple two-variable system which includes the foreign exchange rate
# and real export growth, ordered with the exchange rate shock first


# cut the raw data into a consistent set of dates we want to use to estimate the model
var_data <- data %>%
  dplyr::filter(period >= "1998-01-01" & period <= "2019-12-01")


# estimate two-variable system with select variables
estimate_var(var_data = var_data,
             
             variable_list_in_order = c("real_eer_y_y",
                                        "gs_exp_cvm_y_y"),
             
             variable_names_in_order = c("Exchange rate",
                                         "Real exports"))




# play around with the different forms of the two-variable VAR systems










