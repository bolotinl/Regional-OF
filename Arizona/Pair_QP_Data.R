# Pair area weighted NLDAS precipitation data with USGS streamflow data
# Code author: L Bolotin
library(dataRetrieval) # USGS data package
library(tidyverse) # Data handling
library(glue) # Put together literal strings
library(lubridate) # Handle datetimes

# Set the path for where you want to save your data
setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/Arizona')
sites <- read.csv('./Q_site_info.csv')
sites$site_no <- zeroPad(sites$site_no, 8)
sites <- sites$site_no

# PAIR WITH NLDAS PRECIP DATA -----------------------------------------------------
# Calculated area-weighted P using climrods python tool:
area_weighted_files <- list.files(path = './NLDAS_climrods/area_weighted_P', pattern = '.csv')
nldas_sites <- substr(area_weighted_files, 24, 31)

sites <- sites[sites %in% nldas_sites]
rm(nldas_sites)
run.list <- seq(1, length(sites))

pair_P <- function(x){
  site <- sites[x]
  
  q_path <- glue('Q_{site}')
  q_file <- list.files(path = './Formatted Streamflow Data', pattern = site)
  type <- substr(q_file, 10, 11)
  
  nldas_file <- area_weighted_files[substr(area_weighted_files, 24, 31) == site]
  
  nldas_path <- glue('./NLDAS_climrods/area_weighted_P/{nldas_file}')
  nldas_P <- read.csv(nldas_path)
  nldas_P$DateTime <- ymd_hms(nldas_P$DateTime)
  nldas_P <- nldas_P[complete.cases(nldas_P),]
  colnames(nldas_P)[3] <- 'total_precipitation'
  
  Q_path <- list.files(paste0(getwd(), '/Formatted Streamflow Data/'), pattern = site)
  Q_path <- glue('./Formatted Streamflow Data/{Q_path}')
  Q <- read.csv(Q_path)
  
  if (type == 'uv') {
    
    Q$date <- ymd_hms(Q$date)
    Q <- Q %>% select(-c(X))
    merged_dat <- merge(Q, nldas_P, all.x = TRUE, by.x = 'date', by.y = 'DateTime')
    merged_dat$Q_mm_hr <- ifelse(is.na(merged_dat$Q_mm_hr), paste0('NaN'), paste0(merged_dat$Q_mm_hr))
    merged_dat$QObs_cfs <- ifelse(is.na(merged_dat$QObs_cfs), paste0('NaN'), paste0(merged_dat$QObs_cfs))
    merged_dat$yday <- yday(merged_dat$date)
    
    
    merged_path <- glue('./QP_Data/{site}_QP_{type}.csv')
    write.csv(merged_dat, merged_path)
    
    monsoon_dat <- merged_dat
    monsoon_dat$Q_mm_hr <- ifelse(monsoon_dat$yday <= 156 | monsoon_dat$yday >=257, 'NaN', monsoon_dat$Q_mm_hr)
    monsoon_path <- glue('./Monsoon_Data/{site}_QPmons_{type}.csv')
    write.csv(monsoon_dat, monsoon_path)
    
  } # End of routine for hourly data
  
  if (type == 'dv'){
    Q$date <- ymd(Q$date)
    Q <- Q %>% select(-c(X))
    
    nldas_P_daily <- nldas_P
    nldas_P_daily$day <- date(nldas_P_daily$DateTime)
    nldas_P_daily <- nldas_P_daily %>% 
      group_by(day) %>%
      summarise_at(.vars = c('total_precipitation'), .funs = c(total_precipitation='sum'))
    colnames(nldas_P_daily)[1] <- 'DateTime'
    
    merged_dat <- merge(Q, nldas_P_daily, all.y = TRUE, by.x = 'date', by.y = 'DateTime')
    merged_dat$Q_mm_d <- ifelse(is.na(merged_dat$Q_mm_d), paste0('NaN'), paste0(merged_dat$Q_mm_d))
    merged_dat$QObs_cfs <- ifelse(is.na(merged_dat$QObs_cfs), paste0('NaN'), paste0(merged_dat$QObs_cfs))
    merged_dat$yday <- yday(merged_dat$date)
    
    merged_path <- glue('./QP_Data/{site}_QP_{type}.csv')
    write.csv(merged_dat, merged_path)
    
    monsoon_dat <- merged_dat
    monsoon_dat$Q_mm_d <- ifelse(monsoon_dat$yday <= 156 | monsoon_dat$yday >=257, 'NaN', monsoon_dat$Q_mm_d)
    monsoon_path <- glue('./Monsoon_Data/{site}_QPmons_{type}.csv')
    write.csv(monsoon_dat, monsoon_path)
    
  } # End routine for daily data
  
} # End of pair_P function

lapply(run.list, pair_P)
