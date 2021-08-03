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
root      <- "/ihme/homes/srolfe/projects/rwanda_covid_disruption/"
data_path <- file.path(root, "01_data", "01_raw_data", "data_requested_HMIS_27032021.csv")

#----FUNCTIONS---------------------------------------------------------------------------------------------------------

#--- 2a. PLOT: DOSES ADMINISTERED -------------------------------------------------------------------------------------
# Get prepped data
data <- fread(file.path(root, "01_data", "02_prepped_data", "full_data_with_disruption.csv"))

#--- 2b. PLOT: DISRUPTION RATIO ---------------------------------------------------------------------------------------





#**********************************************************************************************************************


#**********************************************************************************************************************