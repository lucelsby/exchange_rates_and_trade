
### FORECAST ERROR VARIANCE DECOMPOSITION #####################################

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
               tsbox,
               rdbnomics,
               stringr,
               fredr,
               ggpubr,
               purrr,
               RColorBrewer)


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
  dplyr::filter(period >= "1998-01-01" & period <= "2022-07-01")



variable_list_in_order <- c("oil_prod_y_y",
                            "ind_prod_y_y",
                            "oil_price_real_y_y",
                            "real_eer_ln",
                            "gs_exp_cvm_ln")

variable_names_in_order <- c("Oil production",
                            "Real global demand",
                            "Oil price",
                            "Exchange rate",
                            "Real exports")

study_variable <- "gs_exp_cvm_ln"

# extract only relevant columns from VAR data based on variable list and rename columns
var_data_xts <- xts(var_data[,c(variable_list_in_order)],
                    order.by = as.Date(var_data[,c("period")]))


# estimate VAR model 
var_model <- VAR(var_data_xts, 
                 p = 12,
                 type = "const",
                 season = NULL,
                 exog = NULL)


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
ggplot(fevd, aes(x = month, y = fevd, fill = shock)) + 
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










class(var_model)


hd(var_model,)











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

variable_list_in_order <- c("oil_prod_y_y",
                           "ind_prod_y_y",
                           "oil_price_real_y_y",
                           "real_eer_ln",
                           "gs_exp_cvm_ln")

variable_names_in_order <-  c("Oil production",
                            "Real global demand",
                            "Oil price",
                            "Exchange rate",
                            "Real exports")

study_variable <-  "gs_exp_cvm_ln"


var_data_xts <- xts(var_data[,c(variable_list_in_order)],
                    order.by = as.Date(var_data[,c("period")]))

var_model <- VAR(var_data_xts, 
                 p = 12,
                 type = "const",
                 season = NULL,
                 exog = NULL)


# use pre-defined function to estimate historical decomposition 
HD <- VARhd(Estimation=var_model)

# collect historical decomposition for study variable of interest in a dataframe
HD_study_variable <- as.data.frame(HD[,,match(study_variable, variable_list_in_order)])

# combine with date variable
HD_study_variable <- cbind(as.data.frame(time(var_data_xts)),
                           HD_study_variable)

# change column names to the defined variables names
colnames(HD_study_variable) <- c("month", variable_names_in_order)

# convert to long dataframe
HD_study_variable <- HD_study_variable %>%
  pivot_longer(., cols = c(2:ncol(.)),
               names_to = "shock", values_to = "HD") %>%
  dplyr::mutate(HD = HD*100)


# plot
HD_full_sample_plot <- ggplot(HD_study_variable, aes(x=month, y=HD, fill=shock)) +
  geom_area() +
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
  theme(plot.title = element_text(size = 11,face = "bold"),
        plot.subtitle = element_text(size = 9),
        legend.title = element_text(size = 9,face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  guides(fill=guide_legend(title="Structural shock"))


