
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






