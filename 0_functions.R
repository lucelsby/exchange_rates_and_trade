
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
               RColorBrewer,
               viridis)

## Define colour palet for consistency
colour_palet <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                  "#0072B2", "#D55E00", "#CC79A7")



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



## Historical decomposition
VARhd <- function(Estimation){
  
  ## make X and Y
  nlag    <- Estimation$p   # number of lags
  DATA    <- Estimation$y   # data
  QQ      <- VARmakexy(DATA,nlag,1)
  
  
  ## Retrieve and initialize variables 
  invA    <- t(chol(as.matrix(summary(Estimation)$covres)))   # inverse of the A matrix
  Fcomp   <- companionmatrix(Estimation)                      # Companion matrix
  
  #det     <- c_case                                           # constant and/or trends
  F1      <- t(QQ$Ft)                                         # make comparable to notes
  eps     <- ginv(invA) %*% t(residuals(Estimation))          # structural errors 
  nvar    <- Estimation$K                                     # number of endogenous variables
  nvarXeq <- nvar * nlag                                      # number of lagged endogenous per equation
  nvar_ex <- 0                                                # number of exogenous (excluding constant and trend)
  Y       <- QQ$Y                                             # left-hand side
  #X       <- QQ$X[,(1+det):(nvarXeq+det)]                    # right-hand side (no exogenous)
  nobs    <- nrow(Y)                                          # number of observations
  
  
  ## Compute historical decompositions
  
  # Contribution of each shock
  invA_big <- matrix(0,nvarXeq,nvar)
  invA_big[1:nvar,] <- invA
  Icomp <- cbind(diag(nvar), matrix(0,nvar,(nlag-1)*nvar))
  HDshock_big <- array(0, dim=c(nlag*nvar,nobs+1,nvar))
  HDshock <- array(0, dim=c(nvar,(nobs+1),nvar))
  
  for (j in 1:nvar){  # for each variable
    eps_big <- matrix(0,nvar,(nobs+1)) # matrix of shocks conformable with companion
    eps_big[j,2:ncol(eps_big)] <- eps[j,]
    for (i in 2:(nobs+1)){
      HDshock_big[,i,j] <- invA_big %*% eps_big[,i] + Fcomp %*% HDshock_big[,(i-1),j]
      HDshock[,i,j] <-  Icomp %*% HDshock_big[,i,j]
    } 
    
  } 
  
  HD.shock <- array(0, dim=c((nobs+nlag),nvar,nvar))   # [nobs x shock x var]
  
  for (i in 1:nvar){
    
    for (j in 1:nvar){
      HD.shock[,j,i] <- c(rep(NA,nlag), HDshock[i,(2:dim(HDshock)[2]),j])
    }
  }
  
  return(HD.shock)
  
}

VARmakexy <- function(DATA,lags,c_case){
  
  nobs <- nrow(DATA)
  
  #Y matrix 
  Y <- DATA[(lags+1):nrow(DATA),]
  Y <- DATA[-c(1:lags),]
  
  #X-matrix 
  if (c_case==0){
    X <- NA
    for (jj in 0:(lags-1)){
      X <- rbind(DATA[(jj+1):(nobs-lags+jj),])
    } 
  } else if(c_case==1){ #constant
    X <- NA
    for (jj in 0:(lags-1)){
      X <- rbind(DATA[(jj+1):(nobs-lags+jj),])
    }
    X <- cbind(matrix(1,(nobs-lags),1), X) 
  } else if(c_case==2){ # time trend and constant
    X <- NA
    for (jj in 0:(lags-1)){
      X <- rbind(DATA[(jj+1):(nobs-lags+jj),])
    }
    trend <- c(1:nrow(X))
    X <-cbind(matrix(1,(nobs-lags),1), t(trend))
  }
  A <- (t(X) %*% as.matrix(X)) 
  B <- (as.matrix(t(X)) %*% as.matrix(Y))
  
  Ft <- ginv(A) %*% B
  
  retu <- list(X=X,Y=Y, Ft=Ft)
  return(retu)
}

companionmatrix <- function (x) 
{
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
  }
  K <- x$K
  p <- x$p
  A <- unlist(Acoef(x))
  companion <- matrix(0, nrow = K * p, ncol = K * p)
  companion[1:K, 1:(K * p)] <- A
  if (p > 1) {
    j <- 0
    for (i in (K + 1):(K * p)) {
      j <- j + 1
      companion[i, j] <- 1
    }
  }
  return(companion)
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
    scale_fill_manual(values = colour_palet)+
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
  
  
  ## Historical decomposition

  # use pre-defined function to estimate historical decomposition 
  HD <- VARhd(Estimation=var_model)
  
  # collect historical decomposition for study variable of interest in a dataframe
  HD_study_variable <- as.data.frame(HD[,,match(study_variable, variable_list_in_order)])
  
  # combine with date variable
  HD_study_variable <- cbind(as.data.frame(time(var_data_xts)),
                             HD_study_variable)
  
  # change column names to the defined variables names
  colnames(HD_study_variable) <- c("month", variable_names_in_order)
  
  # calculate net value
  net_df <- HD_study_variable %>%
    rowwise %>% 
    mutate(net = sum(c_across(where(is.numeric)))) %>%
    dplyr::select(month, net)
  
  # convert to long dataframe
  HD_study_variable <- HD_study_variable %>%
    pivot_longer(., cols = c(2:ncol(.)),
                 names_to = "shock", values_to = "HD") %>%
    dplyr::mutate(HD = HD) 
  
  
  # plot
  HD_full_sample_plot <- ggplot(data = HD_study_variable) +
    geom_area(aes(x=month, y=HD, fill=shock)) +
    geom_line(data = net_df, mapping = aes(x=month, y=net)) +
    theme_light() +
    scale_fill_manual(values = colour_palet) +
    labs(title = paste0("Historical decomposition of ", 
                        variable_names_in_order[match(study_variable, 
                                                      variable_list_in_order)]),
         subtitle = paste0("Contribution of each structural shock to the cumulative change in ",
                           variable_names_in_order[match(study_variable, 
                                                         variable_list_in_order)]))+
    ylab("%") +
    xlab("") +
    geom_hline(yintercept=0) +
    theme(plot.title = element_text(size = 11,face = "bold"),
          plot.subtitle = element_text(size = 9),
          legend.title = element_text(size = 9,face = "bold"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    guides(fill=guide_legend(title="Structural shock"))
  
  
  
  # return outputs of function as a list
  return(list(irf_plot, fevd_plot, HD_full_sample_plot))
  
  
  
  
}







# function to estimate and plot IRFs at sector level
estimate_sector_irfs <- function(var_data,
                                 variable_extension,
                                 exchange_rate_variable,
                                 other_variables_list){
  
  # create a dataframe to hold impulse respnses from exchange rate variable
  impulse_responses <- NULL
  
  
  # Use the grep function with a regular expression to find all column names that contain "sitc" and the selected variable extension
  variables <- grep(paste0("sitc.*", variable_extension,"*"), 
                    names(var_data), ignore.case = TRUE, value = TRUE)
  
  
  # run loop to estimate a var model and collect IRFs for each sector
  for (i in c(1:length(variables))){
    
    variable <- variables[i]
    
    # extract only relevant columns from VAR data based on variable list and rename columns
    var_data_xts <- xts(var_data[,c(other_variables_list, variable)],
                        order.by = as.Date(var_data[,c("period")]))
    
    
    # estimate VAR model 
    var_model <- VAR(var_data_xts, 
                     p = 12,
                     type = "const",
                     season = NULL,
                     exog = NULL)
    
    
    # estimate impulse response functions of VAR model
    imp <- vars::irf(var_model, impulse = exchange_rate_variable, response = variable,
                     n.ahead = 60, ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
    
    
    # collect central, upper and lower response of study variable to change in the exchange rate variable
    central <- unlist(imp[["irf"]], use.names = FALSE)
    lower <- unlist(imp[["Lower"]], use.names = FALSE)
    upper <- unlist(imp[["Upper"]], use.names = FALSE)
    
    
    # collect period as a numeric variable
    period <- as.numeric(0:(length(central)-1))
    
    
    # combine into data frame
    imp_df <- as.data.frame(cbind(period, central, lower, upper))
    colnames(imp_df) <- c("period", "central", "upper", "lower")
    
    # create a column indicating significance where both upper and lower bounds are different from zero
    imp_df <- imp_df %>%
      
      dplyr::mutate(significant = ifelse((central > 0 & upper > 0 & lower > 0) | (central < 0 & upper < 0 & lower < 0) , 
                                         TRUE, FALSE)) %>%
      
      dplyr::mutate(variable = variable, .before = 1)
    
    
    impulse_responses <- rbind(impulse_responses, imp_df)
    
    
  }
  
  
  
  # extract all unique sitc codes from impulse repsonses
  sitc_codes <- unique(stringr::str_extract(unique(impulse_responses$variable), "(sitc).{2}"))
  
  # create a list to hold all the plots
  plot_list <- list()
  
  
  for (i in c(1:length(sitc_codes))){
    
    sitc_code <- sitc_codes[i]
    
    
    # collect chart data based on sitc code selected
    chart_data <- impulse_responses %>%
      
      # filter to contain just the selected SITC code
      dplyr::filter(stringr::str_detect(variable, sitc_code)) %>%
      
      # create new column based on trade flow
      dplyr::mutate(trade_flow = ifelse(substr(variable, 1, 3) == "exp", "Exports", "Imports")) %>%
      
      # clean the name of the variable column to just contain the SITC name code
      dplyr::mutate(variable = gsub("_", " ", 
                                    toupper(stringr::str_extract(variable, "(sitc).{2}")))) %>%
      
      # merge with SITC names
      dplyr::left_join(sitc_names) %>%
      
      dplyr::select(trade_flow, name, period, central, upper, lower)
    #  pivot_longer(., cols = c(central:lower), names_to = "bound", values_to = "value")
    
    
    title <- unique(chart_data$name)
    
    # assigning plot to plot list
    plot_list[[i]] <- ggplot(chart_data) +
      geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill = trade_flow), alpha=0.5) +
      geom_line(aes(x=period, y=central, color = trade_flow)) +
      labs(title = title) +
      scale_color_manual(values = c(Exports = "red", Imports = "blue")) + 
      scale_fill_manual(values = c(Exports = "pink", Imports = "lightblue")) +
      geom_hline(yintercept = 0, color="black") +
      theme_minimal() +
      ylab("") +
      xlab("") +
      theme(plot.title = element_text(size = 8),
            axis.title.x=element_text(size = 8),
            axis.title.y=element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.title = element_blank())
    
    
  }
  
  # combine plots with a common legend
  irf_plot <- ggarrange(plotlist = plot_list,
                        nrow = sqrt(length(plot_list)),
                        ncol = sqrt(length(plot_list)),
                        common.legend = TRUE, legend = "bottom")
  
  
  # annotate with title
  irf_plot <- annotate_figure(irf_plot, 
                              top = text_grob("Response of trade flows to exchange rate shock across sectrors", 
                                              color = "black", face = "bold", size = 12))
  
  return(irf_plot)
  
  
}





# define function to estimate forecast error variance decomposition (fevd)
# and historical decomposition of trade growth across sectors (growth as it must be stationary)
estimate_sector_decomps <- function(variable_extension,
                                    exchange_rate_variable,
                                    other_variables_list,
                                    other_variable_names,
                                    HD_date,
                                    fevd_horizon){
  
  
  # Use the grep function with a regular expression to find all column names that contain "sitc" and the selected variable extension
  variables <- grep(paste0("sitc.*", variable_extension,"*"), 
                    names(var_data), ignore.case = TRUE, value = TRUE)
  
  
  # run loop to estimate a var model and collect IRFs for each sector
  # for (i in c(1:length(variables))){
  
  # create data frames to hold FEVD and HD for each variable
  fevd_df <- NULL
  hd_df <- NULL
  
  
  
  for (i in c(1:length(variables))){
    
    
    # extract only relevant columns from VAR data based on variable list and rename columns
    var_data_xts <- xts(var_data[,c(other_variables_list, variables[i])],
                        order.by = as.Date(var_data[,c("period")]))
    
    
    # estimate VAR model 
    var_model <- VAR(var_data_xts, 
                     p = 12,
                     type = "const",
                     season = NULL,
                     exog = NULL)
    
    # calculate FEVD of VAR model and extract the FEVD of the study variable of interest
    fevd <- as.data.frame(fevd(var_model, n.ahead = fevd_horizon)[[variables[i]]]) %>%
      
      # create new column with row number as the period
      dplyr::mutate(period = row_number(), .before = 1) %>%
      
      # collect values at the end of the horizon
      tail(., n=1) %>%
      
      # add variable name
      dplyr::mutate(variable = variables[i], .before = 1)
    
    
    # give common variable name
    colnames(fevd)[3:ncol(fevd)] <- c(other_variable_names, "Residual")
    
    
    # combine with dataframe
    fevd_df <- rbind(fevd_df, fevd)
    
    
    ## Historical decomposition
    
    # use pre-defined function to estimate historical decomposition 
    HD <- VARhd(Estimation=var_model)
    
    
    # collect historical decomposition for study variable of interest in a dataframe
    HD <- as.data.frame(HD[,,match(variables[i], colnames(var_data_xts))])
    
    
    # combine with date variable
    HD <- cbind(as.data.frame(time(var_data_xts)),
                HD)
    colnames(HD) <- c("period", colnames(var_data_xts))
    
    
    
    # calculate cumulative contributions to variation over the full sample and the defined date cutoff
    HD_full_sample <- (colSums(abs(HD[, colnames(var_data_xts)]), na.rm=TRUE)/ # total absolute variation caused by each variable
                         sum(abs(unlist(HD[,colnames(var_data_xts)])), na.rm = T))*100 # as percentage of total variation
    
    HD_defined <- (colSums(abs(subset(HD, 
                                      as.Date(period) >= as.Date(HD_date))[, colnames(var_data_xts)]), na.rm=TRUE)/ # total absolute variation caused by each variable
                     sum(abs(unlist(subset(HD, 
                                           as.Date(period) >= as.Date(HD_date))[,colnames(var_data_xts)])), na.rm = T))*100 # as percentage of total variation
    
    
    # bind into a data frame
    HD_bind <- rownames_to_column(as.data.frame(rbind(HD_full_sample, HD_defined)), var = "HD") %>%
      dplyr::mutate(variable = variables[i], .before = 1)
    
    # reassign common variable name 
    colnames(HD_bind)[3:ncol(HD_bind)] <- c(other_variable_names, "Residual")
    
    
    # combine with data frame
    hd_df <- rbind(hd_df, HD_bind)
    
    
    
  }
  
  
  
  
  
  # bubble plot
  # FEVD
  fevd_df <- fevd_df %>%
    pivot_longer(., cols = c(3:ncol(.)), names_to = "shock", values_to = "value") %>%
    
    # create new column based on trade flow
    dplyr::mutate(trade_flow = ifelse(substr(variable, 1, 3) == "exp", "Exports", "Imports")) %>%
    
    # clean the name of the variable column to just contain the SITC name code
    dplyr::mutate(variable = gsub("_", " ", 
                                  toupper(stringr::str_extract(variable, "(sitc).{2}")))) %>%
    
    # merge with SITC names
    dplyr::left_join(sitc_names)
  
  
  
  # Exports
  exports_fevd_df <- fevd_df[fevd_df$trade_flow == "Exports", ]
  exports_fevd <- ggplot(exports_fevd_df, aes(x = shock, y = name)) +
    geom_point(aes(col = value, fill = value, size = value), shape = 21) +
    theme_minimal() +
    guides(
      col = guide_none(),
      size = guide_none(),
      fill = guide_colorbar(
        barheight = unit(0.5, 'cm'),
        barwidth = unit(10, 'cm'),
        title.position = 'top',
        title = "Percent of forecast error variance explained by each shock"
      )
    ) + 
    scale_size_area(max_size = 15) +
    scale_color_viridis(option = "D",direction = -1) +
    scale_fill_viridis(option = "D",direction = -1) +
    scale_y_discrete(limits=rev) +
    labs(title = "Forecast Error Variance Decomposition for Export Growth Across Sectors",
         x= "Shock Type",
         y= "SITC Sector") +
    theme(
      legend.position = 'top',
      text = element_text(color = 'black')
    ) + 
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot") +
    geom_text(aes(label = paste0(round((value*100),0), "%")),
              hjust = 2, vjust = 0, size = 3,
              col = 'grey40')
  
  # Imports
  imports_fevd_df <- fevd_df[fevd_df$trade_flow == "Imports", ]
  imports_fevd <- ggplot(imports_fevd_df, aes(x = shock, y = name)) +
    geom_point(aes(col = value, fill = value, size = value), shape = 21) +
    theme_minimal() +
    guides(
      col = guide_none(),
      size = guide_none(),
      fill = guide_colorbar(
        barheight = unit(0.5, 'cm'),
        barwidth = unit(10, 'cm'),
        title.position = 'top',
        title = "Percent of forecast error variance explained by each shock"
      )
    ) + 
    scale_size_area(max_size = 15) +
    scale_color_viridis(option = "D",direction = -1) +
    scale_fill_viridis(option = "D",direction = -1) +
    scale_y_discrete(limits=rev) +
    labs(title = "Forecast Error Variance Decomposition for Import Growth Across Sectors",
         x= "Shock Type",
         y= "SITC Sector") +
    theme(
      legend.position = 'top',
      text = element_text(color = 'black')
    ) + 
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot") +
    geom_text(aes(label = paste0(round((value*100),0), "%")),
              hjust = 2, vjust = 0, size = 3,
              col = 'grey40')
  
  
  
  
  
  
  # historical decomposition
  hd_df <- hd_df %>%
    pivot_longer(., cols = c(3:ncol(.)), names_to = "shock", values_to = "value") %>%
    
    # create new column based on trade flow
    dplyr::mutate(trade_flow = ifelse(substr(variable, 1, 3) == "exp", "Exports", "Imports")) %>%
    
    # clean the name of the variable column to just contain the SITC name code
    dplyr::mutate(variable = gsub("_", " ", 
                                  toupper(stringr::str_extract(variable, "(sitc).{2}")))) %>%
    
    # merge with SITC names
    dplyr::left_join(sitc_names)
  
  
  # Exports - full sample
  exports_hd_full_sample_df <- hd_df[hd_df$trade_flow == "Exports" & hd_df$HD == "HD_full_sample", ]
  exports_hd_full_sample <- ggplot(exports_hd_full_sample_df, aes(x = shock, y = name)) +
    geom_point(aes(col = value, fill = value, size = value), shape = 21) +
    theme_minimal() +
    guides(
      col = guide_none(),
      size = guide_none(),
      fill = guide_colorbar(
        barheight = unit(0.5, 'cm'),
        barwidth = unit(10, 'cm'),
        title.position = 'top',
        title = "Percent contribution of each structural shock to cumulative change in export growth"
      )
    ) + 
    scale_size_area(max_size = 15) +
    scale_color_viridis(option = "D",direction = -1) +
    scale_fill_viridis(option = "D",direction = -1) +
    scale_y_discrete(limits=rev) +
    labs(title = "Historical Decomposition of Export Growth Across Sectors - Full Sample",
         x= "Shock Type",
         y= "SITC Sector") +
    theme(
      legend.position = 'top',
      text = element_text(color = 'black')
    ) + 
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot") +
    geom_text(aes(label = paste0(round((value),0), "%")),
              hjust = 2, vjust = 0, size = 3,
              col = 'grey40')
  
  
  # Exports - defined date
  exports_hd_defined_df <- hd_df[hd_df$trade_flow == "Exports" & hd_df$HD == "HD_defined", ]
  exports_hd_defined <- ggplot(exports_hd_defined_df, aes(x = shock, y = name)) +
    geom_point(aes(col = value, fill = value, size = value), shape = 21) +
    theme_minimal() +
    guides(
      col = guide_none(),
      size = guide_none(),
      fill = guide_colorbar(
        barheight = unit(0.5, 'cm'),
        barwidth = unit(10, 'cm'),
        title.position = 'top',
        title = "Percent contribution of each structural shock to cumulative change in export growth"
      )
    ) + 
    scale_size_area(max_size = 15) +
    scale_color_viridis(option = "D",direction = -1) +
    scale_fill_viridis(option = "D",direction = -1) +
    scale_y_discrete(limits=rev) +
    labs(title = paste0("Historical Decomposition of Export Growth Across Sectors - since ", HD_date),
         x= "Shock Type",
         y= "SITC Sector") +
    theme(
      legend.position = 'top',
      text = element_text(color = 'black')
    ) + 
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot") +
    geom_text(aes(label = paste0(round((value),0), "%")),
              hjust = 2, vjust = 0, size = 3,
              col = 'grey40')
  
  
  
  # Exports - full sample
  imports_hd_full_sample_df <- hd_df[hd_df$trade_flow == "Imports" & hd_df$HD == "HD_full_sample", ]
  imports_hd_full_sample <- ggplot(imports_hd_full_sample_df, aes(x = shock, y = name)) +
    geom_point(aes(col = value, fill = value, size = value), shape = 21) +
    theme_minimal() +
    guides(
      col = guide_none(),
      size = guide_none(),
      fill = guide_colorbar(
        barheight = unit(0.5, 'cm'),
        barwidth = unit(10, 'cm'),
        title.position = 'top',
        title = "Percent contribution of each structural shock to cumulative change in import growth"
      )
    ) + 
    scale_size_area(max_size = 15) +
    scale_color_viridis(option = "D",direction = -1) +
    scale_fill_viridis(option = "D",direction = -1) +
    scale_y_discrete(limits=rev) +
    labs(title = "Historical Decomposition of Import Growth Across Sectors - Full Sample",
         x= "Shock Type",
         y= "SITC Sector") +
    theme(
      legend.position = 'top',
      text = element_text(color = 'black')
    ) + 
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot") +
    geom_text(aes(label = paste0(round((value),0), "%")),
              hjust = 2, vjust = 0, size = 3,
              col = 'grey40')
  
  
  # Imports - defined date
  imports_hd_defined_df <- hd_df[hd_df$trade_flow == "Imports" & hd_df$HD == "HD_defined", ]
  imports_hd_defined <- ggplot(imports_hd_defined_df, aes(x = shock, y = name)) +
    geom_point(aes(col = value, fill = value, size = value), shape = 21) +
    theme_minimal() +
    guides(
      col = guide_none(),
      size = guide_none(),
      fill = guide_colorbar(
        barheight = unit(0.5, 'cm'),
        barwidth = unit(10, 'cm'),
        title.position = 'top',
        title = "Percent contribution of each structural shock to cumulative change in import growth"
      )
    ) + 
    scale_size_area(max_size = 15) +
    scale_color_viridis(option = "D",direction = -1) +
    scale_fill_viridis(option = "D",direction = -1) +
    scale_y_discrete(limits=rev) +
    labs(title = paste0("Historical Decomposition of Import Growth Across Sectors - since ", HD_date),
         x= "Shock Type",
         y= "SITC Sector") +
    theme(
      legend.position = 'top',
      text = element_text(color = 'black')
    ) + 
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot") +
    geom_text(aes(label = paste0(round((value),0), "%")),
              hjust = 2, vjust = 0, size = 3,
              col = 'grey40')
  
  
  # return objects as a list
  return(list(exports_fevd, imports_fevd, 
              exports_hd_full_sample, exports_hd_defined, 
              imports_hd_full_sample, imports_hd_defined))
  
}


# function to estimate and plot IRFs at aggregate level
estimate_total_irfs <- function(var_data,
                                variable_extension,
                                exchange_rate_variable,
                                other_variables_list){
  
  
  
  # create a dataframe to hold impulse respnses from exchange rate variable
  impulse_responses <- NULL
  
  
  # Use the grep function with a regular expression to find the import and export CVMs with the selected variable extension
  variables <- grep(paste0("g_.*cvm.*", variable_extension,"*"), 
                    names(var_data), ignore.case = TRUE, value = TRUE)
  
  
  # run loop to estimate a var model and collect IRFs for each sector
  for (i in c(1:length(variables))){
    
    variable <- variables[i]
    
    # extract only relevant columns from VAR data based on variable list and rename columns
    var_data_xts <- xts(var_data[,c(other_variables_list, variable)],
                        order.by = as.Date(var_data[,c("period")]))
    
    
    # estimate VAR model 
    var_model <- VAR(var_data_xts, 
                     p = 12,
                     type = "const",
                     season = NULL,
                     exog = NULL)
    
    
    # estimate impulse response functions of VAR model
    imp <- vars::irf(var_model, impulse = exchange_rate_variable, response = variable,
                     n.ahead = 60, ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.68, runs = 500)
    
    
    # collect central, upper and lower response of study variable to change in the exchange rate variable
    central <- unlist(imp[["irf"]], use.names = FALSE)
    lower <- unlist(imp[["Lower"]], use.names = FALSE)
    upper <- unlist(imp[["Upper"]], use.names = FALSE)
    
    
    # collect period as a numeric variable
    period <- as.numeric(0:(length(central)-1))
    
    
    # combine into data frame
    imp_df <- as.data.frame(cbind(period, central, lower, upper))
    colnames(imp_df) <- c("period", "central", "upper", "lower")
    
    # create a column indicating significance where both upper and lower bounds are different from zero
    imp_df <- imp_df %>%
      
      dplyr::mutate(significant = ifelse((central > 0 & upper > 0 & lower > 0) | (central < 0 & upper < 0 & lower < 0) , 
                                         TRUE, FALSE)) %>%
      
      dplyr::mutate(variable = variable, .before = 1)
    
    
    impulse_responses <- rbind(impulse_responses, imp_df)
    
    
  }
  
  
  # collect chart data based on sitc code selected
  chart_data <- impulse_responses %>%
    
    # create new column based on trade flow
    dplyr::mutate(trade_flow = ifelse(substr(variable, 3, 5) == "exp", "Exports", "Imports")) %>%
    
    dplyr::select(trade_flow, period, central, upper, lower)
  
  
  # plot
  irf_plot <- ggplot(chart_data) +
    geom_ribbon(aes(x=period, ymin=lower, ymax=upper, fill = trade_flow), alpha=0.5) +
    geom_line(aes(x=period, y=central, color = trade_flow)) +
    labs(title = "Impuls Respons Function of UK Trade to an Exchage Rate Shock") +
    scale_color_manual(values = c(Exports = "red", Imports = "blue")) + 
    scale_fill_manual(values = c(Exports = "pink", Imports = "lightblue")) +
    geom_hline(yintercept = 0, color="black") +
    theme_minimal() +
    ylab("Percent") +
    xlab("Month Horizon") +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x=element_text(size = 10),
          axis.title.y=element_text(size = 10),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank())
  
  return(irf_plot)
  
  
}





