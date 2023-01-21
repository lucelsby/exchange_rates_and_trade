
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






# inputs
other_variables_list <- c("oil_prod_y_y",
                            "ind_prod_y_y",
                            "oil_price_real_y_y",
                            "real_eer_ln")
other_variables_name <- c("Oil production",
                             "Real global demand",
                             "Oil price",
                             "Exchange rate")
variable_extension <- "_ln"





# define functionality to take the defined study variable and collect the sector equivalent series
# then run a var and IRF and plot all the IRFs of each sector into a single chart (if possible)
# then run a historical decomposition and plot a heatmap




# step 1, extract all relevant variables from study variable
# Create a vector of all column names in the data frame
col_names <- names(var_data)

# Use the grep function with a regular expression to find all column names that contain "sitc" and "_ln"
sitc_ln_cols <- grep("sitc.*_ln", col_names, ignore.case = TRUE, value = TRUE)

# Print the resulting vector
print(sitc_ln_cols)





# extract only relevant columns from VAR data based on variable list and rename columns
var_data_xts <- xts(var_data[,c(variable_list_in_order)],
                    order.by = as.Date(var_data[,c("period")]))


# estimate VAR model 
var_model <- VAR(var_data_xts, 
                 p = 12,
                 type = "const",
                 season = NULL,
                 exog = NULL)


# estimate impulse response functions of VAR model
imp <- vars::irf(var_model,
                 n.ahead = 60,
                 ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)








