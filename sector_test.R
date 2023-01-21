
## sector test

# clear environment
rm(list = ls())
graphics.off()
options(scipen = 999)


# source the functions R script to load pre-defined functions
source("0_functions.R")


# read in data
data <- read.csv("clean_data/data.csv")


# cut the raw data into a consistent set of dates we want to use to estimate the model
var_data <- data %>%
  dplyr::filter(period >= "1998-01-01" & period <= "2019-12-01")


# read in sitc names
sitc_names <- readxl::read_excel("input_data/sitc_names.xlsx",col_names = FALSE)
colnames(sitc_names) <- c("name")
sitc_names <- sitc_names %>%
  dplyr::mutate(variable = substr(name, 1, 6))




# estimate sector IRFs
barp <- estimate_sector_irfs(var_data = var_data,
                             variable_extension = "_ln",
                             exchange_rate_variable = "real_eer_ln",
                             other_variables_list = c("oil_prod_y_y",
                                                      "ind_prod_y_y",
                                                      "oil_price_real_y_y",
                                                      "real_eer_ln"))



# forecast variance error decomposition









# historical decomposition

















