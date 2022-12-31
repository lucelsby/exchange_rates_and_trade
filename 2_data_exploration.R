
### DATA EXPLORATION ###########################################################

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









