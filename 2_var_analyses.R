
### TWO-VARIABLE VAR MODEL #####################################################

# [Description of what this script does]



## Setup Environment -----------------------------------------------------------
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



# simple two variable VAR
# in growth rates
two_growth <- estimate_var(var_data = var_data,
                           variable_list_in_order = c("real_eer_y_y",
                                                      "gs_exp_cvm_y_y"),
                           variable_names_in_order = c("Real exchange rate growth",
                                                       "Real export growth"),
                           study_variable = "gs_exp_cvm_y_y")

two_growth[[1]]
two_growth[[2]]
two_growth[[3]]

# in levels
two_level <- estimate_var(var_data = var_data,
                           variable_list_in_order = c("real_eer_ln",
                                                      "gs_exp_cvm_ln"),
                           variable_names_in_order = c("Real exchange rate",
                                                       "Real export"),
                           study_variable = "gs_exp_cvm_ln")

two_level[[1]]
two_level[[2]]
two_level[[3]]



# five variable VAR
# in growth rates
five_growth <- estimate_var(var_data = var_data,
                           variable_list_in_order = c("oil_prod_y_y",
                                                      "ind_prod_y_y",
                                                      "oil_price_real_y_y",
                                                      "real_eer_y_y",
                                                      "gs_exp_cvm_y_y"),
                           
                           variable_names_in_order = c("Oil production growth",
                                                       "Real global demand growth",
                                                       "Oil price growth",
                                                       "Exchange rate growth",
                                                       "Real export growth"),
                           
                           study_variable = "gs_exp_cvm_y_y")

five_growth[[1]]
five_growth[[2]]
five_growth[[3]]

# in levels
five_level <- estimate_var(var_data = var_data,
                          variable_list_in_order = c("oil_prod_y_y",
                                                     "ind_prod_y_y",
                                                     "oil_price_real_y_y",
                                                     "real_eer_ln",
                                                     "gs_exp_cvm_ln"),
                          
                          variable_names_in_order = c("Oil production",
                                                      "Real global demand",
                                                      "Oil price",
                                                      "Exchange rate",
                                                      "Real exports"),
                          
                          study_variable = "gs_exp_cvm_ln")

five_level[[1]]
five_level[[2]]
five_level[[3]]














## Iwaisako and Nakata replication ---------------------------------------------

# we start with a simple two-variable system which includes the foreign exchange rate
# and real export growth, ordered with the exchange rate shock first

# year on year growth
# estimate_var(var_data = var_data,
#              
#              variable_list_in_order = c("real_eer_y_y",
#                                         "gs_exp_cvm_y_y"),
#              
#              variable_names_in_order = c("Exchange rate",
#                                          "Real exports"))
# 
# # month on month growth
# estimate_var(var_data = var_data,
#              
#              variable_list_in_order = c("real_eer_m_m",
#                                         "gs_exp_cvm_m_m"),
#              
#              variable_names_in_order = c("Exchange rate",
#                                          "Real exports"))
# 
# # level ER and real exports in logs
# estimate_var(var_data = var_data,
#              
#              variable_list_in_order = c("real_eer_ln",
#                                         "gs_exp_cvm_ln"),
#              
#              variable_names_in_order = c("Exchange rate",
#                                          "Real exports"))
# 
# 
# 
# 
# # like the look of level terms variables so perhaps proceed with them but get views from tutor.
# # https://forums.eviews.com/viewtopic.php?t=11275
# # There is an issue of whether the variables in a VAR need to be stationary. Sims (1980) and Sims, Stock and Watson (1990) recommend against differencing even if the variables contain a unit root. They argued that the goal of a VAR analysis is to determine the interrelationships among the variables, not to determine the parameter estimates. The main argument against differencing is that it "throws away" information concerning the comovements in the data (such as the possibility of cointegrating relationships). Similary, it is argued that the data need not be detrended. In a VAR, a trending variable will be well approximated by a unit root plus drift. However, majority view is that the form of variables in the VAR should mimic the true data-generating process. This is particularly true if the aim is to estimate a structural model.
# 
# 
# # now we move onto a three-variable specification with a proxy for global real economic activity
# 
# # level industrial production, real ER and real exports
# estimate_var(var_data = var_data,
#              
#              variable_list_in_order = c("ind_prod_ln",
#                                         "real_eer_ln",
#                                         "gs_exp_cvm_ln"),
#              
#              variable_names_in_order = c("Real global demand",
#                                          "Exchange rate",
#                                          "Real exports"))
# 
# 
# # now we move onto a five-variable specification, adding oil price and oil production
# estimate_var(var_data = var_data,
#              
#              variable_list_in_order = c("oil_prod_ln",
#                                         "ind_prod_ln",
#                                         "oil_price_real_ln",
#                                         "real_eer_ln",
#                                         "gs_exp_cvm_ln"),
#              
#              variable_names_in_order = c("Oil production",
#                                          "Real global demand",
#                                          "Oil price",
#                                          "Exchange rate",
#                                          "Real exports"))



## Next steps

# add functionality to take the sitc sectors of the defined export/import series
# and calculate the forecast error variance decomposition of each sector
# then display in a heatmap the relative importance 
# could also calculate cumulative contribution of shocks of past 10 years/20 years/5 years and put into a heatmap


