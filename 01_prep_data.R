#----HEADER------------------------------------------------------------------------------------------------------------
#' Author:      srolfe
#' Date:        August 2021
#' Background:  Gates Ventures received admin1-level monthly routine immunization data for Rwanda. 
#'              Investigate disruption ratio (COVID) and determine whether data is suitable for
#'              mobility modeling (using Kate Causey's cascade method)
#' Script:     *01_prep_data.R*
#'              02_time_series_analysis.R              
#**********************************************************************************************************************
library(data.table)
library(magrittr)

#----GLOBAL VARIABLES--------------------------------------------------------------------------------------------------
# File Paths
root      <- "/ihme/homes/srolfe/projects/rwanda_covid_disruption/"
data_path <- file.path(root, "01_data", "01_raw_data", "data_requested_HMIS_27032021.csv")

# Column names
column_names <- c("organisationunitname"     = "district",
                  "periodname"               = "month_year",                                                      
                  # "Pneumococus 1"            = "pcv1",                                                    
                  # "Pneumococus 3"            = "pcv3",                                                    
                  # "Rotavirus 1"              = "rota1",                                                      
                  # "Rotavirus 2"              = "rota3",                                        
                  "Measles & Rubella (MR) 1" = "mr1",                                         
                  # "Measles & Rubella (MR) 2" = "mr2",                                         
                  # "Polio 3"                  = "polio3",                                                          
                  # "BCG"                      = "bcg",                                                              
                  "DTP_HepB_Hib3"            = "penta3")

# Month numbers
month_number <- c("January"   = 1, 
                  "February"  = 2, 
                  "March"     = 3, 
                  "April"     = 4, 
                  "May"       = 5, 
                  "June"      = 6, 
                  "July"      = 7, 
                  "August"    = 8, 
                  "September" = 9, 
                  "October"   = 10, 
                  "November"  = 11, 
                  "December"  = 12)

#----FUNCTIONS---------------------------------------------------------------------------------------------------------
# Parse month or year from "<Month> <Year>"
get_date <- function(month_year, type) {
  # Parse month_year string in to vector of c(month, year)
  month_year_split <- strsplit(month_year, " ") %>% unlist()
  # Get date of interest (month or year)
  if(type == "month") {
    date <- month_year_split[[1]]
  } else if (type == "year") {
    date <- as.integer(month_year_split[[2]])
  } else {
    stop("Please specify 'month' or 'year' in 'type' argument")
  }
  return(date)
}
 
#--- 1. GET DATA ------------------------------------------------------------------------------------------------------
# Get data
data <- fread(data_path)

# Subset to indicators of interest and rename columns
data <- subset(data, select = names(column_names))
setnames(data, old = names(column_names), new = unname(column_names))

# Get dates and convert months to numeric
data$month      <- lapply(data$month_year, get_date, "month") %>% unlist()
data$year       <- lapply(data$month_year, get_date, "year") %>% unlist()
data$month_year <- NULL
data[, month := month_number[month]]
setcolorder(data, c("district", "month", "year"))

# Get DPT3 from Penta3 and drop penta
data$dpt3   <- data$penta3
data$penta3 <- NULL

#--- 2. GET DISRUPTION RATIO ------------------------------------------------------------------------------------------
# Get 2020/2019 pre-pandemic disruption
pre_pandemic_2019 <- data[year == 2019 & month %in% c(1, 2), .(mr1_prepandemic_2019  = sum(mr1),
                                                               dpt3_prepandemic_2019 = sum(dpt3)), 
                          by = c("district")]
pre_pandemic_2020 <- data[year == 2020 & month %in% c(1, 2), .(mr1_prepandemic_2020  = sum(mr1),
                                                               dpt3_prepandemic_2020 = sum(dpt3)), 
                          by = c("district")]

pre_pandemic <- merge(pre_pandemic_2019, pre_pandemic_2020, 
                      by = c("district"), all = TRUE)

pre_pandemic <- pre_pandemic[, .(district, 
                                 year = 2020,
                                 mr1_pre_pandemic_ratio = mr1_prepandemic_2020 / mr1_prepandemic_2019, 
                                 dpt3_pre_pandemic_ratio = dpt3_prepandemic_2020 / dpt3_prepandemic_2019)]


# 2020/2019 pandemic disruption (normal and cumulative)
pandemic <- data[year %in% c(2019, 2020) & month %in% 3:12, ]
pandemic[, mr1_cumulative := cumsum(mr1), by = c("district", "year")]
pandemic[, dpt3_cumulative := cumsum(dpt3), by = c("district", "year")]

pandemic <- dcast(pandemic, "district + month ~ year", 
                  value.var = (c("mr1", "dpt3", "mr1_cumulative", "dpt3_cumulative")))
pandemic <- pandemic[, .(district, 
                         month, 
                         year = 2020,
                         mr1_2019, 
                         mr1_2020,
                         mr1_pandemic_ratio  = mr1_2020 / mr1_2019,
                         dpt3_2019, 
                         dpt3_2020,
                         dpt3_pandemic_ratio = dpt3_2020 / dpt3_2019,
                         mr1_cumulative_2019,
                         mr1_cumulative_2020,
                         mr1_cumulative_pandemic_ratio = mr1_cumulative_2020 / mr1_cumulative_2019,
                         dpt3_cumulative_2019, 
                         dpt3_cumulative_2020,
                         dpt3_cumulative_pandemic_ratio = dpt3_cumulative_2020 / dpt3_cumulative_2019)]

# Combine pre-pandemic disruption with pandemic disruption 
disruption <- merge(pre_pandemic, pandemic, 
                    by = c("district", "year"), 
                    all.x = TRUE, all.y = TRUE)

# Calculate disruption ratio as (pandemic ratio / pre-pandemic ratio) in normal and cumulative space
disruption <- disruption[, .(district, 
                             month, 
                             year, 
                             mr1_2019, 
                             mr1_2020, 
                             mr1_pre_pandemic_ratio,
                             mr1_pandemic_ratio, 
                             mr1_disruption_ratio = mr1_pandemic_ratio / mr1_pre_pandemic_ratio, 
                             mr1_cumulative_disruption_ratio = mr1_cumulative_pandemic_ratio / mr1_pre_pandemic_ratio,
                             dpt3_2019, 
                             dpt3_2020, 
                             dpt3_pre_pandemic_ratio,
                             dpt3_pandemic_ratio, 
                             dpt3_disruption_ratio = dpt3_pandemic_ratio / dpt3_pre_pandemic_ratio, 
                             dpt3_cumulative_disruption_ratio = dpt3_cumulative_pandemic_ratio / dpt3_pre_pandemic_ratio)]

# Combine disruption ratio data with original data for fully prepped dataset
full_data <- merge(data, disruption, 
                   by = c("district", "month", "year"), 
                   all.x = TRUE, all.y = TRUE)
full_data[, `:=`(mr1_2019  = NULL, 
                 mr1_2020  = NULL,
                 dpt3_2019 = NULL,
                 dpt3_2020 = NULL)]

#--- 3. SAVE DATA -----------------------------------------------------------------------------------------------------

# Save prepped data
fwrite(data, file = file.path(root, "01_data", "02_prepped_data", "prepped_data.csv"))
fwrite(full_data, file = file.path(root, "01_data", "02_prepped_data", "prepped_data_with_disruption.csv"))
fwrite(disruption, file = file.path(root, "01_data", "02_prepped_data", "disruption.csv"))

#**********************************************************************************************************************