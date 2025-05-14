# Rwanda Immunization Data Analysis

This repository contains R scripts used for processing and analyzing routine immunization data for Rwanda, with a focus on understanding the impact of COVID-19 on service delivery.

## 📄 Files

- **`01_prep_data.R`**  
  Calculate disruption ratio for monthly routine immunization data for Rwanda.

- **`02_time_series_analysis.R`**  
  Plot time series data to assess the disruption ratio caused by COVID-19 on routine immunization coverage.

## 📊 Overview

The analysis focuses on:
- Structuring raw immunization data for time series evaluation
- Quantifying COVID-induced disruption in service provision
- Visualizing coverage trends over time

## 📦 Requirements

This project uses R. Key packages likely needed include:
- `data.table`
- `magrittr`
- `ggplot2`

Install them via:
```r
install.packages(c("tidyverse", "lubridate", "ggplot2"))
