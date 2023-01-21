
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




# estimate sector IRFs
barp <- estimate_sector_irfs(var_data = var_data,
                             variable_extension = "_ln",
                             exchange_rate_variable = "real_eer_ln",
                             other_variables_list = c("oil_prod_y_y",
                                                      "ind_prod_y_y",
                                                      "oil_price_real_y_y",
                                                      "real_eer_ln"))










### FEVD and HD

# inputs
variable_extension = "_ln"
exchange_rate_variable = "real_eer_y_y"
other_variables_list = c("oil_prod_y_y",
                         "ind_prod_y_y",
                         "oil_price_real_y_y",
                         "real_eer_y_y")
other_variable_names= c("Oil production",
                          "Real global demand",
                          "Oil price",
                          "Exchange rate")












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
  fevd <- as.data.frame(fevd(var_model, n.ahead = 60)[[variables[i]]]) %>%
    
    # create new column with row number as the period
    dplyr::mutate(period = row_number(), .before = 1) %>%
    
    # collect values at the 12 and 60 month horizon
    dplyr::filter(period == 12 | period == 60)
  
  
  # combine this with the FEVD at a very large horizon as a proxy for the infinite horizon
  fevd <- rbind(fevd, (as.data.frame(fevd(var_model, n.ahead = 1000)[[variables[i]]]) %>%
                         # collect last row to show FEVD in final period
                         dplyr::mutate(period = row_number(), .before = 1) %>% tail(., n=1))) %>%
    
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
  
  
  
  # calculate cumulative contributions to variation over last year, 5 years and full sample
  HD_full_sample <- (colSums(abs(HD[, colnames(var_data_xts)]), na.rm=TRUE)/ # total absolute variation caused by each variable
                       sum(abs(unlist(HD[,colnames(var_data_xts)])), na.rm = T))*100 # as percentage of total variation
  
  
  HD_last_year <- (colSums(abs(tail(HD, n=12)[, colnames(var_data_xts)]), na.rm=TRUE)/ # total absolute variation caused by each variable
                     sum(abs(unlist(tail(HD, n=12)[,colnames(var_data_xts)])), na.rm = T))*100 # as percentage of total variation
  
  
  HD_five_year <- (colSums(abs(tail(HD, n=60)[, colnames(var_data_xts)]), na.rm=TRUE)/ # total absolute variation caused by each variable
                     sum(abs(unlist(tail(HD, n=60)[,colnames(var_data_xts)])), na.rm = T))*100 # as percentage of total variation
  
  
  # bind into a data frame
  HD_bind <- rownames_to_column(as.data.frame(rbind(HD_last_year, HD_five_year, HD_full_sample)), var = "HD") %>%
    dplyr::mutate(variable = variables[i], .before = 1)
  
  # reassign common variable name 
  colnames(HD_bind)[3:ncol(HD_bind)] <- c(other_variable_names, "Residual")
  
  
  # combine with data frame
  hd_df <- rbind(hd_df, HD_bind)
  
  
  
}
  




# bubble plot
# FEVD - 1000


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
exports_fevd <- fevd_df[fevd_df$trade_flow == "Exports" & fevd_df$period == 1000, ]
ggplot(exports_fevd, aes(x = shock, y = name)) +
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
  labs(title = "Forecast Error Variance Decomposition for Export Sectors",
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
imports_fevd <- fevd_df[fevd_df$trade_flow == "Imports" & fevd_df$period == 1000, ]
ggplot(imports_fevd, aes(x = shock, y = name)) +
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
  labs(title = "Forecast Error Variance Decomposition for Import Sectors",
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

















