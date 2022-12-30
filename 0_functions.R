
### FUNCTIONS ##################################################################

# [Description of what this script does]

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
               purrr,
               RColorBrewer)


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



## Define a function to plot Impulse Response Functions (to be used later) -----
plot_irfs <- function(imp, # estimated impulse response function of VAR
                      
                      j, # impulse variable
                      
                      k, # response variable
                      
                      variable_names # vector of variable names for plots
                      
                      ) {
  
  # collect estimates of IRF responses from impulse as data frames
  central_responses <- as.data.frame(imp$irf[[j]])
  lower_responses <- as.data.frame(imp$Lower[[j]])
  upper_responses <- as.data.frame(imp$Upper[[j]])
  
  
  # collect response of select response variable
  central <- central_responses[,k]
  lower <- lower_responses[,k]
  upper <- upper_responses[,k]
  
  
  # collect period as a numeric variable
  period <- as.numeric(0:(length(central)-1))
  
  
  # combine into data frame
  imp_df <- as.data.frame(cbind(period, central, lower, upper))
  colnames(imp_df) <- c("period", "central", "upper", "lower")
  
  
  # plot
  imp_df %>%
    ggplot(aes(x= period, y = central, ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0, color="black") +
    geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
    geom_line() +
    theme_light() +
    labs(title = paste0(variable_names[j], " to ", variable_names[k], " Shock")) + 
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 8),
          axis.title.x=element_text(size = 8),
          axis.title.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  
  
}





estimate_var <- function(var_data = var_data, # var data input in data frame format - can contain all variables (default var_data)
                         
                         variable_list_in_order, # vector of variable names to be included in the VAR in order
                         
                         variable_names_in_order, # list of variable names to be displayed on plots
                         
                         study_variable # variable of interest for FEVD
                         
                         ) {
  
  
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
  
  
  
  ## plot IRFs of VAR all in one page by running loop
  # extract variable names in character vector
  var_names <- imp$response
  
  
  # extract number of variables in VAR from character vector
  n_vars <- length(var_names)
  
  
  # total number of plots needed to create is equal to the number of variables squared
  n_plots <- n_vars^2
  
  
  # create a sequence of impulse variables - each variable is an impulse for the total amount of variables
  impulse_sequence <- rep(1:n_vars, each = n_vars)
  
  
  # create a sequence of response variables
  response_sequence <- rep(seq(1,n_vars,1),n_vars)
  
  
  plot_list <- list()
  
  # run loop to assign a plot to each impulse and each response
  for (i in c(1:n_plots)) {
    
    # define the impulse variable for the shock based on position in sequence
    j <- impulse_sequence[i]
    
    # define the response variable for the shock based on position in sequence
    k <- response_sequence[i]
    
    # plot variables using pre-defined function
    plot_list[[i]] <- plot_irfs(imp = imp, j = j, k = k,
                                variable_names = variable_names_in_order)
    
    
        
  }
  
  irf_plot <- ggarrange(plotlist = plot_list,
            nrow = n_vars,
            ncol = n_vars)
  
  ## FEVD
  
  # calculate FEVD of VAR model and extract the FEVD of the study variable of interest
  fevd <- as.data.frame(fevd(var_model, n.ahead = 60)[[match(study_variable, variable_list_in_order)]])
  
  
  # plot stacked area chart
  # change column names based on variable names
  colnames(fevd) <- variable_names_in_order
  
  # add month horizon based on row number 
  fevd <- fevd %>%
    dplyr::mutate(month = row_number(), .before = 1)
  
  # convert to long data frame and add 'shock' after each variable name
  fevd <- fevd %>%
    pivot_longer(., cols = c(2:ncol(.)), 
                 names_to = "shock", values_to = "fevd") %>%
    dplyr::mutate(shock = paste0(shock, " shock")) %>%
    dplyr::mutate(fevd = fevd*100)
  
  # plot
  fevd_plot <- ggplot(fevd, aes(x = month, y = fevd, fill = shock)) + 
    geom_area(position = 'stack') +
    theme_light() +
    scale_fill_brewer(palette = "Blues") +
    labs(title = paste0("Forecast error variance decomposition for ", 
                        variable_names_in_order[match(study_variable, 
                                                      variable_list_in_order)]),
         subtitle = paste0("Proportion of variability in ",
                           variable_names_in_order[match(study_variable, 
                                                         variable_list_in_order)],
                           " explained by shocks to given variables"))+
    ylab("Proportion (%)") +
    xlab("Month horizon") +
    theme(plot.title = element_text(size = 11,face = "bold"),
          plot.subtitle = element_text(size = 9),
          legend.title = element_text(size = 9,face = "bold"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    guides(fill=guide_legend(title="Shock Variable"))
  
  return(list(irf_plot, fevd_plot))
  
}








