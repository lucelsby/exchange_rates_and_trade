
### FUNCTIONS ##################################################################

# [Description of what this script does]



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



## Define function to plot Impulse Response Functions (IRF) --------------------

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



## Define function to estimate and plot two-variable VAR system ----------------

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



## Define function to estimate and plot three-variable VAR system --------------

estimate_three_variable_var <- function(var_data,
                                        var1,
                                        var2,
                                        var3){
  
  # extract only relevant columns and rename colnames
  var_data_xts <- xts(var_data[,c(var1, var2, var3)],
                      order.by = as.Date(var_data[,c("period")]))
  colnames(var_data_xts) <- c("var1", "var2", "var3")
  
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











var1 <- "real_eer_y_y"
var2 <- "gs_exp_cvm_y_y"
var3 <- "oil_price_real_y_y"


# extract only relevant columns and rename colnames
var_data_xts <- xts(var_data[,c(var1, var2, var3)],
                    order.by = as.Date(var_data[,c("period")]))


colnames(var_data_xts) <- c("var1", "var2", "var3")


# estimate model with 12 lags
var_model <- VAR(var_data_xts, 
                        p = 12,
                        type = "const",
                        season = NULL,
                        exog = NULL)

summary(var_model)


imp <- vars::irf(var_model,
                  n.ahead = 60,
                  ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)



var_data <- var_data
variable_list_in_order <- c("oil_price_real_y_y",
                            "ind_prod_y_y",
                            "real_eer_y_y",
                            "gs_exp_cvm_y_y")
n_lags <- 12
n_ahead <- 60



estimate_var <- function(var_data = var_data, # var data input in data frame format - can contain all variables (default var_data)
                         
                         variable_list_in_order, # vector of variable names to be included in the VAR in order
                         
                         n_lags = 12, # number of lags to be included - default is 12
                         
                         n_ahead = 60 # number of forecast periods - default is 60 (5 years)
                         
                         ) {
  
  
  # extract only relevant columns from VAR data based on variable list and rename columns
  var_data_xts <- xts(var_data[,c(variable_list_in_order)],
                      order.by = as.Date(var_data[,c("period")]))
  
  
  # estimate VAR model 
  var_model <- VAR(var_data_xts, 
                   p = n_lags,
                   type = "const",
                   season = NULL,
                   exog = NULL)
  
  
  # print out summary of model
  summary(var_model)
  
  
  # plot roots of VAR model (should be less than 1)
  par(mfrow = c(1,1))
  plot(roots(var_model, modulus = T), xlab = "Eigenvalues", ylab = "")
  
  
  # estimate impulse response functions of VAR model
  imp <- vars::irf(var_model,
                   n.ahead = n_ahead,
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
  
  
  # run loop to assign a plot to each impulse and each response
  for (i in c(1:n_plots)) {
    
    # define the impulse variable for the shock based on position in sequence
    j <- impulse_sequence[i]
    
    # define the response variable for the shock based on position in sequence
    k <- response_sequence[i]
    
    assign(
      
      # name the variable based on the sequence of plot numbers
      paste0("p_", i),
      
      # run function on data based on impulse sequence and response sequence
      plot_irfs(imp = imp, j = j, k = k)
      
      )
        
  }
  
  plot_list <- paste0("p_", seq(1, n_plots, 1))
  ggarrange(plotlist = plot_list,
            nrow = n_vars,
            ncol = n_vars)
  
}




# arrange a plot based on the number of variables in the VAR


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


i <- 2
j <- 1
k <- 2

assign(
  
  # name the variable based on the sequence of plot numbers
  paste0("p_", i),
  
  # run function on data based on impulse sequence and response sequence
  plot_irfs(imp = imp, j = j, k = k)
  
       )



# next step is to find a way of plotting the list of plots in a grid arrange format





grid.arrange(p2, p5, p3, p6, ncol = 2,
             top = text_grob("UK Goods and Services Trade", size = 11, face = "bold"),
             bottom = text_grob("Source: Office for National Statistics (ONS)", 
                                size = 11, face = "italic", hjust = 2))


plot_list <- paste0("p_", seq(1, n_plots, 1))

plot_list <- map(paste0("p_", seq(1, n_plots, 1)),myplot)

cowplot::plot_grid(plot_list, ncol = n_vars)







## 
plot_irfs <- function(imp, j, k){
  
  # collect estimates of IRF responses from impulse as data frames
  central_responses <- as.data.frame(imp$irf[[j]])
  lower_responses <- as.data.frame(imp$Lower[[j]])
  upper_responses <- as.data.frame(imp$Upper[[j]])
  
  
  # collect response of select response variable
  # collect central, upper and lower confidence estimates
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
    labs(title = "") + # add in later on
    ylab("")+
    xlab("Months") +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.x=element_text(size = 9),
          axis.title.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
}







