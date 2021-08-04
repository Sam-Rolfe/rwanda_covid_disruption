#----HEADER------------------------------------------------------------------------------------------------------------
#' Author:      srolfe
#' Date:        August 2021
#' Background:  Gates Ventures received admin1-level monthly routine immunization data for Rwanda. 
#'              Investigate disruption ratio (COVID) and determine whether data is suitable for
#'              mobility modeling (using Kate Causey's cascade method)
#' Script:      01_prep_data.R
#'             *02_time_series_analysis.R*              
#' Explanation: Month-Region specific time-series plots of
#'              1) Doses administered (MR1 and DPT3)
#'              2) Disruption ratio (MR1 and DPT3) (Normal and Cumulative space)              
#**********************************************************************************************************************
library(data.table)
library(ggplot2)

#----GLOBAL VARIABLES--------------------------------------------------------------------------------------------------
# File Paths
project_root               <- "/ihme/homes/srolfe/projects/rwanda_covid_disruption/"
data_root                  <- file.path(project_root, "01_data", "02_prepped_data")
diagnostic_root            <- file.path(project_root, "03_diagnostics")

prepped_data_path          <- file.path(data_root, "prepped_data.csv")
disruption_ratio_data_path <- file.path(data_root, "disruption.csv")


#----FUNCTIONS---------------------------------------------------------------------------------------------------------

#--- 1. GET DATA: DOSES ADMINISTERED --------------------------------------------------------------------------------
# Get prepped data
data            <- fread(prepped_data_path)
disruption_data <- fread(disruption_ratio_data_path, 
                         select = c("district", "month", "year", 
                                    "mr1_disruption_ratio", "mr1_cumulative_disruption_ratio", 
                                    "dpt3_disruption_ratio", "dpt3_cumulative_disruption_ratio"))


# Add date
data[, date := as.Date(paste("2020", month, "01", sep = "-"), format = "%Y-%m-%d")] # Intentionally set 2020 as default
disruption_data[, date := as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")]

# Format dose data for overlayed comparison of years
data$year <- factor(data$year, levels = rev(2017:2020))

# Get district for ease of plotting
districts   <- sort(unique(data$district))
n_districts <- length(districts)

# Identify districts per panel
districts_per_panel <- 6
n_panels            <- n_districts / districts_per_panel

# Get values used in plotting (y-max)
y_max_mr1  <- max(data$mr1)
y_max_dpt3 <- max(data$dpt3)

y_max_disruption  <- max(c(disruption_data$mr1_cumulative_disruption_ratio, 
                           disruption_data$dpt3_cumulative_disruption_ratio, 
                           disruption_data$mr1_disruption_ratio, 
                           disruption_data$dpt3_disruption_ratio))
y_min_disruption  <- min(c(disruption_data$mr1_cumulative_disruption_ratio, 
                           disruption_data$dpt3_cumulative_disruption_ratio, 
                           disruption_data$mr1_disruption_ratio, 
                           disruption_data$dpt3_disruption_ratio))
#--- 2. PLOT: DOSES ADMINISTERED ------------------------------------------------------------------------------------

# Open PDF for plotting
pdf(file = file.path(diagnostic_root, "time_series_analysis.pdf"))

# Plot MR1 doses administered for each district in n_panels
for(i in seq.int(n_panels)) {
  
  # Get districts of interest
  index_district_start  <- ((i - 1) * 6) + 1
  index_district_end    <- i * 6 
  districts_of_interest <- districts[index_district_start:index_district_end] 
  
  gg <- ggplot(data[district %in% districts_of_interest & year %in% c(2019, 2020)], mapping = aes(x = date, y = mr1, color = year)) +
    geom_point() +
    geom_line() +
    # scale_colour_viridis_d() +
    # ylim(c(0, y_max_mr1)) +
    labs(color = "Year") +
    ylab("Doses Administered") +
    scale_x_date(date_labels = "%b", 
                 date_breaks  ="2 month") +
    ggtitle("MR1: Doses Administered") +
    facet_wrap(~ district, 
               scales = "free_y") +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  plot(gg)
  
}


# Plot DPT3 doses administered for each district in n_panels
for(i in seq.int(n_panels)) {
  
  # Get districts of interest
  index_district_start  <- ((i - 1) * 6) + 1
  index_district_end    <- i * 6 
  districts_of_interest <- districts[index_district_start:index_district_end] 
  
  gg <- ggplot(data[district %in% districts_of_interest & year %in% c(2019, 2020)], mapping = aes(x = date, y = dpt3, color = year)) +
    geom_point() +
    geom_line() +
    # scale_colour_viridis_d() +
    # ylim(c(0, y_max_dpt3)) +
    labs(color = "Year") +
    ylab("Doses Administered") +
    scale_x_date(date_labels = "%b", 
                 date_breaks  ="2 month") +
    ggtitle("DPT3: Doses Administered") +
    facet_wrap(~ district, 
               scales = "free_y") +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))  
  plot(gg)
}



#--- 3. PLOT: DISRUPTION RATIO - NORMAL -----------------------------------------------------------------------------

# Plot MR1 disruption ratio for each district in n_panels
for(i in seq.int(n_panels)) {
  
  # Get districts of interest
  index_district_start  <- ((i - 1) * 6) + 1
  index_district_end    <- i * 6 
  districts_of_interest <- districts[index_district_start:index_district_end] 
  
  gg <- ggplot(disruption_data[district %in% districts_of_interest], mapping = aes(x = date, y = mr1_disruption_ratio)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 1, 
               linetype = "dashed", 
               color = "red") +
    ylim(c(y_min_disruption, 
           y_max_disruption)) +
    ylab("Disruption Ratio") +
    scale_x_date(date_labels = "%b", 
                 date_breaks  ="1 month") +
    ggtitle("MR1: Disruption Ratio") +
    facet_wrap(~ district) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  plot(gg)
  
}


# Plot DPT3 disruption ratio for each district in n_panels
for(i in seq.int(n_panels)) {
  
  # Get districts of interest
  index_district_start  <- ((i - 1) * 6) + 1
  index_district_end    <- i * 6 
  districts_of_interest <- districts[index_district_start:index_district_end] 
  
  gg <- ggplot(disruption_data[district %in% districts_of_interest], mapping = aes(x = date, y = dpt3_disruption_ratio)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 1, 
               linetype = "dashed", 
               color = "red") +
    ylim(c(y_min_disruption, 
           y_max_disruption)) +    ylab("Disruption Ratio") +
    scale_x_date(date_labels = "%b", 
                 date_breaks  ="1 month") +
    ggtitle("DPT3: Disruption Ratio") +
    facet_wrap(~ district) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  plot(gg)
  
}




#--- 4. PLOT: DISRUPTION RATIO - CUMULATIVE --------------------------------------------------------------------


# Plot MR1 cumulative disruption ratio for each district in n_panels
for(i in seq.int(n_panels)) {
  
  # Get districts of interest
  index_district_start  <- ((i - 1) * 6) + 1
  index_district_end    <- i * 6 
  districts_of_interest <- districts[index_district_start:index_district_end] 
  
  gg <- ggplot(disruption_data[district %in% districts_of_interest], mapping = aes(x = date, y = mr1_cumulative_disruption_ratio)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 1, 
               linetype = "dashed", 
               color = "red") +
    ylim(c(y_min_disruption, 
           y_max_disruption)) +
    ylab("Disruption Ratio (Cumulative)") +
    scale_x_date(date_labels = "%b", 
                 date_breaks  ="1 month") +
    ggtitle("MR1: Disruption Ratio (Cumulative)") +
    facet_wrap(~ district) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  plot(gg)
  
}


# Plot DPT3 disruption ratio for each district in n_panels
for(i in seq.int(n_panels)) {
  
  # Get districts of interest
  index_district_start  <- ((i - 1) * 6) + 1
  index_district_end    <- i * 6 
  districts_of_interest <- districts[index_district_start:index_district_end] 
  
  gg <- ggplot(disruption_data[district %in% districts_of_interest], mapping = aes(x = date, y = dpt3_cumulative_disruption_ratio)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 1, 
               linetype = "dashed", 
               color = "red") +
    ylim(c(y_min_disruption, 
           y_max_disruption)) +
    ylab("Disruption Ratio (Cumulative)") +
    scale_x_date(date_labels = "%b", 
                 date_breaks  ="1 month") +
    ggtitle("DPT3: Disruption Ratio (Cumulative)") +
    facet_wrap(~ district) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  plot(gg)
}

# Close PDF
dev.off()


#**********************************************************************************************************************


