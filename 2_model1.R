
### MODEL 1 ####################################################################

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
               tsbox)


# clear environment
rm(list = ls())
graphics.off()
options(scipen = 999)


# read in data
data <- read.csv("clean_data/data_model1.csv")



## Define function to plot time series -----------------------------------------

plot_series <- function(data,
                        variable,
                        start_date,
                        end_date,
                        fig_title){
  
  
  # collect date variable
  period <- data[,"period"]
  
  # collect series
  series <- data[,variable]
  
  # bind into a data frame in the right format
  timeseries <- as.data.frame(cbind(period, series)) %>%
    dplyr::mutate(period = as.Date(period), series = as.numeric(series)) %>%
    dplyr::filter(period >= as.Date(start_date) & period <= as.Date(end_date))
  
  # plot time series
  plot <- ggplot(timeseries, aes(x = period, y = series)) +
    geom_line(colour = "#000099") +
    theme_bw() +
    labs(title = fig_title) +
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  return(plot)
  
}



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


## 2-Variable System -----------------------------------------------------------

# we start with a simple two-variable system which includes the foreign exchange rate
# and real export growth, ordered with the exchange rate shock first

# cut the raw data into a consistent set of dates we want to use to estimate the model
var_data <- data %>%
  dplyr::filter(period >= "1998-01-01" & period <= "2019-12-01")


# define function to plot Impulse Response Functions (IRF)
plot_irf <- function(imp,
                     title){
  
  # collect central, upper and lower confidence estimates
  central <- imp$irf[[1]]
  lower <- imp$Lower[[1]]
  upper <- imp$Upper[[1]]
  
  # collect period as a numeric variable
  period <- as.numeric(0:(length(central)-1))
  
  # combine into dataframe
  imp_df <- as.data.frame(cbind(period, central, lower, upper))
  colnames(imp_df) <- c("period", "central", "upper", "lower")
  
  # plot
  imp_df %>%
    ggplot(aes(x= period, y = central, ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, color="black") +
    geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
    geom_line() +
    theme_light() +
    labs(title = title)+
    ylab("")+
    xlab("Months") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.x=element_text(size = 9),
          axis.title.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}

var_data <- var_data
var1 <- "real_eer_y_y"
var2 <- "gs_exp_cvm_y_y"

# define function to run and plot outputs of two-variable VAR system
estimate_two_variable_var <- function(var_data,
                                      var1,
                                      var2){
  
  # extract only relevant columns and rename colnames
  var_data_xts <- xts(var_data[,c(var1, var2)],
      order.by = as.Date(var_data[,c("period")]))
  colnames(var_data_xts) <- c("var1", "var2")
  
  # estimate model with 12 lags
  two_variable_var <- VAR(var_data_xts, 
                          p = 12,
                          type = "const",
                          season = NULL,
                          exog = NULL)
  
  # print summary
  summary(two_variable_var)
  
  
  # plot roots of VAR model (should be less than 1)
  par(mfrow = c(1,1))
  plot(roots(two_variable_var, modulus = T), xlab = "Eigenvalues", ylab = "")

  
  # estimate IRF of impulses of variable 1
  # var1 on itself
  irf_var1_var1 <- vars::irf(two_variable_var, impulse = "var1",
                             response=cbind("var1"), 
                             n.ahead = 60,
                             ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
  # var1 on var2
  irf_var1_var2 <- vars::irf(two_variable_var, impulse = "var1",
                             response=cbind("var2"), 
                             n.ahead = 60,
                             ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
  
  # estimate IRF of impulses of variable 2
  # var2 on var1
  irf_var2_var1 <- vars::irf(two_variable_var, impulse = "var2",
                             response=cbind("var1"), 
                             n.ahead = 60,
                             ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
  # var2 on itself
  irf_var2_var2 <- vars::irf(two_variable_var, impulse = "var2",
                             response=cbind("var2"), 
                             n.ahead = 60,
                             ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
  
  # plot IRFs
  p1 <- plot_irf(imp = irf_var1_var1, 
                 title = paste0(var1, " to ", var1, " Shock")) +
    theme(axis.title.x=element_blank())
  
  p2 <- plot_irf(imp = irf_var1_var2, 
                 title = paste0(var1, " to ", var2, " Shock")) +
    theme(axis.title.x=element_blank())
  
  p3 <- plot_irf(imp = irf_var2_var1, 
                 title = paste0(var2, " to ", var1, " Shock"))
  
  p4 <- plot_irf(imp = irf_var2_var2, 
                 title = paste0(var2, " to ", var2, " Shock"))
  
  
  # plot all IRFs in grid
  grid.arrange(p1, p2, p3, p4, ncol = 2,
               top = text_grob("Impulse Response Functions", size = 11, face = "bold"),
               bottom = text_grob("Grey area indicates 68% confidence interval", 
                                  size = 10, face = "italic"))
  
}


# estimate two-variable system with select variables
estimate_two_variable_var(var_data = var_data,
                          var1 = "real_eer_y_y",
                          var2 = "gs_exp_cvm_y_y")




# simplify code by creating a functions script
# seperate out into data collection, data exploration and then the three different var systems (2, 3, 5)










