
### DATA COLLECTION ############################################################

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
               fredr)


# clear environment
rm(list = ls())
graphics.off()
options(scipen = 999)


# source the functions R script to load pre-defined functions
source("0_functions.R")


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
  "BQKQ", # UK exports of just goods to the world - chain volume measure
  
  ## effective exchange rate
  "BK67",
  
  ## Trade in Goods by Sector Data - volume measures
  "JIM5",
  "JIM4",
  "ODUM",
  "OGSH",
  "OEBB",
  "OGSM",
  "OEDZ",
  "OGSP",
  "BOXC",
  "BPIC",
  "OEEX",
  "OGSU",
  "ENDW",
  "ENGQ",
  "OEPR",
  "OGSZ",
  "OGRN",
  "OGTG",
  "OGSE",
  "OGTM"

  )


# define a list of variable names to correspond to each series (must be in order)
mret_names <- c(
  ## goods and services data
  "gs_imp",
  "gs_exp",
  "gs_imp_cvm",
  "gs_exp_cvm",
  
  ## purely goods data
  "g_imp",
  "g_exp",
  "g_imp_cvm",
  "g_exp_cvm",
  
  ## effective exchange rate
  "eer",
  
  ## Trade in Goods by Sector Data - volume measures
  "exp_excl_pm",
  "imp_excl_pm",
  "exp_sitc_0",
  "imp_sitc_0",
  "exp_sitc_1",
  "imp_sitc_1",
  "exp_sitc_2",
  "imp_sitc_2",
  "exp_sitc_3",
  "imp_sitc_3",
  "exp_sitc_4",
  "imp_sitc_4",
  "exp_sitc_5",
  "imp_sitc_5",
  "exp_sitc_6",
  "imp_sitc_6",
  "exp_sitc_7",
  "imp_sitc_7",
  "exp_sitc_8",
  "imp_sitc_8"

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




## Upload oil market data from input files -------------------------------------
# two bits of oil-related data are collected from the US Energy Information Administration website
# global oil production and US oil price
# these have both been downloaded and stored in the input_data file

# upload oil production data from raw csv file
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
eia_oil_production_clean <- as.data.frame(cbind(period, as.numeric(oil_prod)))

# convert character to date format
eia_oil_production_clean$period <- as.Date(eia_oil_production_clean$period)

# clean names
colnames(eia_oil_production_clean) <- c("period", "oil_prod")


# upload oil price data from raw excel file
eia_oil_price_clean <- janitor::clean_names(readxl::read_excel("input_data/20221126_eia_oil_price.xls", 
                                                               sheet = "Data 1", skip = 2))

# clean column names
colnames(eia_oil_price_clean) <- c('period', 'oil_price')


# set dates to the first day of the month
eia_oil_price_clean <- eia_oil_price_clean %>%
  
  dplyr::mutate(period = paste0(substr(period, 1, 7), "-01"))

# convert to date variable
eia_oil_price_clean$period <- as.Date(eia_oil_price_clean$period)


## Download data from FRED -----------------------------------------------------
# We need two series that can be downloaded from FRED
# UK economic policy uncertainty
# US CPI inflationto deflate the oil price

# set FRED API key for current R session
fredr_set_key("6180003aeadbfbb097bf2b324b98540a")


# retrieve UK economic policy uncertainty index between selected dates
fred_epu_clean <- fredr(
  series_id = "UKEPUINDXM",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-10-01")
  ) %>%
  
  # clean
  dplyr::select(date, value) %>%
  
  dplyr::rename(period = date,
                epu = value)


# retrieve US CPI data between selected dates
fred_cpi_clean <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-10-01")
) %>%
  
  # clean
  dplyr::select(date, value) %>%
  
  dplyr::rename(period = date,
                cpi_index = value)  


# retrieve UK real effective exchange rate between selected dates
# this comes from FRED but is taken from the Bank of International Settlements
fred_real_eer_clean <- fredr(
  series_id = "RBGBBIS",
  observation_start = as.Date("1995-01-01"),
  observation_end = as.Date("2022-10-01")
) %>%
  
  # clean
  dplyr::select(date, value) %>%
  
  dplyr::rename(period = date,
                real_eer = value)


## Merge data into single data frame -------------------------------------------

# define list of all relevant data frames to merge
mylist <- list(eia_oil_price_clean, eia_oil_production_clean, 
               ons_data_clean, oecd_industrial_production_clean,
               fred_cpi_clean, fred_epu_clean, fred_real_eer_clean)

# merge together
data <- Reduce(function(...) merge(..., by="period", all=TRUE), mylist) %>% 
  dplyr::arrange(period)









## Data cleaning ---------------------------------------------------------------

data <- data %>%
  
  # make sure all variables are numeric
  mutate_if(is.character,as.numeric) %>%
  
  # deflate oil price by the US CPI index
  dplyr::mutate(oil_price_real = oil_price/(cpi_index/100)) %>%
  
  # generate implied deflators for ONS series 
  dplyr::mutate(gs_imp_def = (gs_imp/gs_imp_cvm)*100,
                gs_exp_def = (gs_exp/gs_exp_cvm)*100,
                g_imp_def = (g_imp/g_imp_cvm)*100,
                g_exp_def = (g_exp/g_exp_cvm)*100) %>%
  
  # take the log of all variables
  mutate(across(where(is.numeric), 
                funs(chg = log(.)), 
                .names = "{col}_ln")) %>%
  
  # calculate year on year growth rates for all numeric variables
  mutate(across(where(is.numeric), 
                funs(chg = ((.-lag(., n = 12))/lag(., n = 12))*100), 
                .names = "{col}_y_y")) %>%
  
  # calculate period on period growth rates for all numeric variables
  mutate(across(where(is.numeric), 
                funs(chg = ((.-lag(.))/lag(.))*100), 
                .names = "{col}_m_m")) %>%
  
  # remove all the columns where all 3 are calculated
  dplyr::select(-contains("_ln_y_y")) %>%
  dplyr::select(-contains("_ln_m_m")) %>%
  dplyr::select(-contains("_y_y_m_m"))
  
  


## Store Data in Files ---------------------------------------------------------

# store in clean data folder with time stamp
data %>%
  write_csv(., file = paste0("clean_data/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data.csv"))


# store in clean data folder without time stamp so it can always be used as the latest data
data %>%
  write_csv(., file = paste0("clean_data/", "data.csv"))









## Data Exploration ------------------------------------------------------------



## Plot ONS trade data ---------------------------------------------------------

start_date <- "1998-01-01"
end_date <- "2022-01-01"

# Create plots for exports and imports: value, CVM and price
p1 <- plot_series(data = data,
                  variable = "gs_exp",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Exports")

p2 <- plot_series(data = data,
                  variable = "gs_exp_cvm",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Real exports")

p3 <- plot_series(data = data,
                  variable = "gs_exp_def_y_y",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Exports implied year-on-year deflator growth") + geom_hline(yintercept=0)

p4 <- plot_series(data = data,
                  variable = "gs_imp",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Imports")

p5 <- plot_series(data = data,
                  variable = "gs_imp_cvm",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Real imports")

p6 <- plot_series(data = data,
                  variable = "gs_imp_def_y_y",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Imports implied year-on-year deflator growth") + geom_hline(yintercept=0)


# plot real imports and exports as well as their deflator growth
grid.arrange(p2, p5, p3, p6, ncol = 2,
             top = text_grob("UK Goods and Services Trade", size = 11, face = "bold"),
             bottom = text_grob("Source: Office for National Statistics (ONS)", 
                                size = 11, face = "italic", hjust = 2))



## Plot other variables to be included in the model ----------------------------

# real effective exchange rate
p1 <- plot_series(data = data,
                  variable = "real_eer",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Real Sterling effective exchange eate")


# growth rate of crude oil production
p2 <- plot_series(data = data,
                  variable = "oil_prod_y_y",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Growth rate of global oil production")


# growth rate of OECD members industrial production
p3 <- plot_series(data = data,
                  variable = "ind_prod_y_y",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Growth rate of OECD member industrial production")


# crude oil price
p4 <- plot_series(data = data,
                  variable = "oil_price_real",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Price of crude oil")


# plot other variables
grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = text_grob("Other Explanatory Variables", size = 11, face = "bold"),
             bottom = text_grob("Source: ONS, St. Louis Fed, OECD and US Energy Information Administration (EIA)", 
                                size = 11, face = "italic", hjust = 1))


## Stationary Tests ------------------------------------------------------------

# remove NA's from the dataset
df <- data %>% drop_na()

# set critical value for rejection of the NULL hypothesis of non-stationary
critical_value <- 0.05

# define empty vector to hold results of stationary tests for all variables
stationary_results_table <- NULL


# for all variables except the date variable, calculate the ADF test p-value
for (i in c(2:ncol(df))) {
  
  # extract each time series
  x <- df[,i]
  
  # collect variable name
  variable_name <- names(df[i])
  
  # calculate ADF test p-value
  p_value <- adf.test(x)$p.value
  
  # set stationary equal to TRUE when the null hypothesis is rejected (i.e., variable is stationary)
  stationary = p_value < critical_value
  
  # collect variable name, p-value and stationary classification result
  results <- cbind(variable_name, p_value, stationary)
  
  # bind onto results table
  stationary_results_table <- as.data.frame(rbind(stationary_results_table, results))
  
}

