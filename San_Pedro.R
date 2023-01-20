# Download and format data for the San Pedro River Watershed
library(dataRetrieval)
library(tidyverse)
library(glue)
library(lubridate)

setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River')

# Identify sites by huc 8 basin (Upper & Lower San Pedro)
site_query <- whatNWISsites(parameterCd = '00060', huc = c('15050202', '15050203'))
site_query <- site_query %>% filter(site_tp_cd == 'ST') # streams only

sites <- unique(data_query$site_no)

# See what kind of data is available at these sites
data_query <- whatNWISdata(siteNumbers = sites)
data_query <- data_query %>% filter(parm_cd == '00060') # discharge only
data_query <- data_query %>% filter(data_type_cd != 'qw') # logger data only, not field obs

# Output site info
write.csv(site_query, './Q_site_info.csv')

## Download data --------------------------------------------------------------
download_Q <- function(x){
  
site <- sites[x]
data <- data_query %>% filter(site_no == site)
types <- data$data_type_cd

# If sub-daily data is available, download it. If not, download daily data.
if ('uv' %in% types) {
  q <- readNWISuv(site, '00060', '', '') # Grabs full period of available data
} else {
 q <- readNWISdv(site, '00060', '', '') # Grabs full period of available data
}

type <- ifelse('uv' %in% types, 'uv', 'dv')

q_path <- glue('./Q_{site}_{type}.csv')
write.csv(q, q_path)
}

run.list <- seq(length(sites))
# Apply download function to all sites:
lapply(run.list, download_Q)

## Format Data ----------------------------------------------------------------
format_Q <- function(x) {

site <- sites[x]
q_path <- glue('Q_{site}')
q_file <- list.files(path = getwd(), pattern = q_path)

q <- read.csv(q_file)
type <- substr(q_file, 12, 13)

if (type == 'uv') {
  
  q$dateTime <- ymd_hms(q$dateTime)
  
  # Some sub-daily data is 15-min. Average these to hourly.
  q$hour <- paste0(substr(q$dateTime, 1, 13), ':00:00')
  q_hourly <- q %>%
    group_by(hour) %>%
    summarise_at(.vars = c('X_00060_00000'), .funs = c('q_cfs_hourly' = mean))
  q_hourly$hour <- ymd_hms(q_hourly$hour)
  
  # We need all timestamps to calculate the signatures, even if they are missing data (fill with NaN)
  q_start <- q_hourly$hour[1]
  q_end <- q_hourly$hour[length(q_hourly$hour)]
  full_TS <- seq.POSIXt(q_start, q_end, by = 'hour')
  full_TS <- full_TS %>% as.data.frame()
  
  q_hourly_full <- merge(q_hourly, full_TS, by.x = 'hour', by.y = '.', all.y = TRUE)
  
  colnames(q_hourly_full) <- c('date', 'QObs_cfs')
  
  # Get drainage area in meters squared
  site_info <- readNWISsite(site)
    da <- site_info$drain_area_va #mi2
  da_m2 <- da*2589988.11 #m2 
  
  # Get flow in cubic meters per second
  # Comment for now and try the same approach used in thesis code
  # q_hourly_full$QObs_mmh <- as.numeric(q_hourly_full$QObs_cfs)*0.0283168 #m3/s
  # q_hourly_full$QObs_mmh <- q_hourly_full$QObs_mmh/da_m2 #m/s
  # q_hourly_full$QObs_mmh <- q_hourly_full$QObs_mmh*3600 #mm/hr
  # 
  # q_hourly_full$QObs_mmh <- ifelse(is.na(q_hourly_full$QObs_mmh), paste0('NaN'), paste0(q_hourly_full$QObs_mmh))
  
  drainage_area <- da*27878400 # sq mi to sq ft
  q_hourly_full_test <- q_hourly_full
  q_hourly_full_test$Q_ft_s <- q_hourly_full_test$QObs_cfs/drainage_area 
  q_hourly_full_test$Q_mm_hr <- q_hourly_full_test$Q_ft_s*304.8*3600
  # This give more normal numbers.
  
  q_path <- glue('./Formatted_Q/{site}_uv.csv')
  write.csv(q_hourly_full_test, q_path)
  
} # End of if statement

if (type == 'dv') {
  q$Date <- ymd(q$Date)
 
  q_start <- q$Date[1]
  q_end <- q$Date[length(q$Date)]
  full_TS <- seq.Date(q_start, q_end, by = 'day')
  full_TS <- full_TS %>% as.data.frame()
  
  q_full <- merge(q, full_TS, by.x = 'Date', by.y = '.', all.y = TRUE)
  q_full <- q_full %>% select(c(Date, X_00060_00003))
  
  colnames(q_full) <- c('date', 'QObs_cfs')
  
  site_info <- readNWISsite(site)
  
  # Get drainage area in meters squared
  da <- site_info$drain_area_va #mi2
  da_m2 <- da*2589988.11#m2
  
  # Get flow in cubic meters per second
  # Comment for now and try approach used in thesis codes
  # q_full$QObs_mmd <- as.numeric(q_full$QObs_cfs)*0.0283168 #m3/s
  # q_full$QObs_mmd <- q_full$QObs_mmd/da_m2 #m/s
  # q_full$QObs_mmd <- q_full$QObs_mmd*86400 #mm/d
  # 
  # q_full$QObs_mmd <- ifelse(is.na(q_full$QObs_mmd), paste0('NaN'), paste0(q_full$QObs_mmd))
  
  drainage_area <- da*27878400 # sq mi to sq ft
  q_full_test <- q_full
  q_full_test$Q_ft_s <- q_full_test$QObs_cfs/drainage_area 
  q_full_test$Q_mm_d <- q_full_test$Q_ft_s*304.8*86400
  # This give more normal numbers.
  
  q_path <- glue('./Formatted_Q/{site}_dv.csv')
  write.csv(q_full, q_path)
  } # End of if statement
} # End of function

lapply(run.list, format_Q)

# PAIR WITH NLDAS PRECIP DATA -----------------------------------------------------
# For now, pair each gauge with the NLDAS grid cell it is in

nldas <- read.csv('./NLDAS_intersections.csv')
nldas$NLDAS_ID <- paste0('X', zeroPad(nldas$NLDAS_X, 3), '-', 'Y', zeroPad(nldas$NLDAS_Y, 3))

pair_P <- function(x){
  site <- sites[x]
  
  q_path <- glue('Q_{site}')
  q_file <- list.files(path = getwd(), pattern = q_path)
  type <- substr(q_file, 12, 13)
  
  nldas_id <- nldas$NLDAS_ID[zeroPad(nldas$site_no, 8) == site]
  
  nldas_path <- glue('./NLDAS Precip/APCPsfc_{nldas_id}.csv')
  nldas_P <- read.csv(nldas_path)
  nldas_P <- separate(nldas_P, 1, c('DateTime', 'total_precipitation'), sep = ' (?=[^ ]+$)')   
  nldas_P$DateTime <- paste0(substr(nldas_P$DateTime, 7, 19), ':00:00')
  nldas_P$DateTime <- ymd_hms(nldas_P$DateTime)
  nldas_P <- nldas_P[complete.cases(nldas_P),]
  nldas_P$total_precipitation <- as.numeric(as.character(nldas_P$total_precipitation))
  
  Q_path <- list.files(paste0(getwd(), '/Formatted_Q/'), pattern = site)
  Q_path <- glue('./Formatted_Q/{Q_path}')
  Q <- read.csv(Q_path)
  
  if (type == 'uv') {
    
  Q$date <- ymd_hms(Q$date)
  Q <- Q %>% select(-c(X))
  merged_dat <- merge(Q, nldas_P, all.y = TRUE, by.x = 'date', by.y = 'DateTime')
  merged_dat$QObs_mmh <- ifelse(is.na(merged_dat$QObs_mmh), paste0('NaN'), paste0(merged_dat$QObs_mmh))
  merged_dat$QObs_cfs <- ifelse(is.na(merged_dat$QObs_cfs), paste0('NaN'), paste0(merged_dat$QObs_cfs))
  merged_dat$yday <- yday(merged_dat$date)
  
  
  merged_path <- glue('./QP_Data/{site}_QP_{type}.csv')
  write.csv(merged_dat, merged_path)
  
  monsoon_dat <- merged_dat
  monsoon_dat$QObs_mmh <- ifelse(monsoon_dat$yday <= 156 | monsoon_dat$yday >=257, 'NaN', monsoon_dat$QObs_mmh)
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
    merged_dat$QObs_mmd <- ifelse(is.na(merged_dat$QObs_mmd), paste0('NaN'), paste0(merged_dat$QObs_mmd))
    merged_dat$QObs_cfs <- ifelse(is.na(merged_dat$QObs_cfs), paste0('NaN'), paste0(merged_dat$QObs_cfs))
    merged_dat$yday <- yday(merged_dat$date)
    
    
    merged_path <- glue('./QP_Data/{site}_QP_{type}.csv')
    write.csv(merged_dat, merged_path)
    
    monsoon_dat <- merged_dat
    monsoon_dat$QObs_mmd <- ifelse(monsoon_dat$yday <= 156 | monsoon_dat$yday >=257, 'NaN', monsoon_dat$QObs_mmd)
    monsoon_path <- glue('./Monsoon_Data/{site}_QPmons_{type}.csv')
    write.csv(monsoon_dat, monsoon_path)
    
  } # End routine for daily data
  
  } # End of pair_P function
  

lapply(run.list, pair_P)
