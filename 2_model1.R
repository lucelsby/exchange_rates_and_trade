
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
               tseries)


# clear environment
rm(list = ls())
graphics.off()
options(scipen = 999)


# read in data
data <- read.csv("clean_data/20221127_173559_data_model1.csv")



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

# effective exchange rate
p1 <- plot_series(data = data,
                  variable = "eer",
                  start_date = start_date,
                  end_date = end_date,
                  fig_title = "Sterling effective exchange eate")


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


multi_stat_tests<- function(df){
  
  # drop nas
  df <- df %>% drop_na() %>% dplyr::select(-period)
  
  # convert data frame into a time series object
  df <- xts(df[,-1], order.by=as.Date(df[,1]))
  
  # collect number of columns
  p <- ncol(df)
  
  # for all variables collect the Box test, ADF test an KPSS test p values
  df_multi <- data.frame(var=names(df),
                         box.pvalue=sapply(df, function(v) Box.test(ts(v),lag=20,type="Ljung-Box")$p.value),
                         adf.pvalue=sapply(df, function(v) adf.test(ts(v),alternative = "stationary")$p.value),
                         kpss.pvalue=sapply(df, function(v) kpss.test(ts(v))$p.value)
  )
  
  # report true in data frame if they pass the test
  df_multi$box <- df_multi$box.pvalue < 0.05
  df_multi$adf <- df_multi$adf.pvalue < 0.05
  df_multi$kpss <- df_multi$kpss.pvalue > 0.05
  
  
  # print data frame
  print(df_multi)
  
  # return data frame
  print(df_multi)
  
}


test_results <- multi_stat_tests(data)






