
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


# read in sitc names
sitc_names <- readxl::read_excel("input_data/sitc_names.xlsx",col_names = FALSE)
colnames(sitc_names) <- c("name")
sitc_names <- sitc_names %>%
  dplyr::mutate(variable = substr(name, 1, 6))







# create a dataframe to hold impulse respnses from exchange rate variable
impulse_responses <- NULL




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
exchange_rate_variable <- "real_eer_ln"







# define functionality to take the defined study variable and collect the sector equivalent series
# then run a var and IRF and plot all the IRFs of each sector into a single chart (if possible)
# then run a historical decomposition and plot a heatmap




# step 1, extract all relevant variables from study variable
# Create a vector of all column names in the data frame
col_names <- names(var_data)

# Use the grep function with a regular expression to find all column names that contain "sitc" and "_ln"
variables <- grep("sitc.*_ln", col_names, ignore.case = TRUE, value = TRUE)

# Print the resulting vector
print(variables)







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
  
  # assing plot to plot list
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
annotate_figure(irf_plot, top = text_grob("Response of trade flows to exchange rate shock across sectrors", 
                                      color = "black", face = "bold", size = 12))






















