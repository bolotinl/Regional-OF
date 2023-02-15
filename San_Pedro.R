# Download and format data for the San Pedro River Watershed
library(dataRetrieval)
library(tidyverse)
library(glue)
library(lubridate)

setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River')

# Identify sites by huc 8 basin (Upper & Lower San Pedro)
site_query <- whatNWISsites(parameterCd = '00060', huc = c('15050202', '15050203'))
site_query <- site_query %>% filter(site_tp_cd == 'ST') # streams only
sites <- unique(site_query$site_no)


# See what kind of data is available at these sites
data_query <- whatNWISdata(siteNumbers = sites)
data_query <- data_query %>% filter(parm_cd == '00060') # discharge only
data_query <- data_query %>% filter(data_type_cd != 'qw') # logger data only, not field obs
data_query <- data_query %>% filter(end_date > '1980-01-01') # NLDAS doesn't start until 1980

site_query <- site_query %>% filter(site_no %in% data_query$site_no)
sites <- unique(site_query$site_no)

# Output site info
write.csv(site_query, './Q_site_info.csv')

## Download data --------------------------------------------------------------
download_Q <- function(x){
  
site <- sites[x]
data <- data_query %>% filter(site_no == site)
types <- data$data_type_cd

# If sub-daily data is available, download it. If not, download daily data.
if ('uv' %in% types) {
  q <- readNWISuv(site, '00060', '1980-01-01', '') # Grabs full period of available data after NLDAS becomes available
} else {
 q <- readNWISdv(site, '00060', '1980-01-01', '') 
}

type <- ifelse('uv' %in% types, 'uv', 'dv')

q_path <- glue('./Streamflow Data/Q_{site}_{type}.csv')
write.csv(q, q_path)
}

run.list <- seq(length(sites))

# Apply download function to all sites:
# lapply(run.list, download_Q) # Comment after running once to avoid re-downloading

## Format Data ----------------------------------------------------------------
format_Q <- function(x) {

site <- sites[x]
q_path <- glue('Q_{site}')
q_file <- list.files(path = '/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River/Streamflow Data', pattern = q_path)

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
  
  # Calculate specific discharge
  site_info <- readNWISsite(site)
  da <- site_info$drain_area_va # sq mi
  drainage_area <- da*27878400 # sq mi to sq ft
  
  q_hourly_full$Q_ft_s <- q_hourly_full$QObs_cfs/drainage_area 
  q_hourly_full$Q_mm_hr <- q_hourly_full$Q_ft_s*304.8*3600

  q_path <- glue('./Formatted_Q/{site}_uv.csv')
  write.csv(q_hourly_full, q_path)
  
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
  
  # Calculate specific discharge
  site_info <- readNWISsite(site)
  da <- site_info$drain_area_va # sq mi
  if (is.na(da)) {
    watershed_areas <- read.csv('watershed_areas.csv')
    da <- watershed_areas$AREA[zeroPad(watershed_areas$GAGE_ID,8) == site] # sq m
    da <- da * 3.861022e-7 #sq mi
  }
  drainage_area <- da*27878400 # sq mi to sq ft
  
  q_full$Q_ft_s <- q_full$QObs_cfs/drainage_area 
  q_full$Q_mm_d <- q_full$Q_ft_s*304.8*86400

  q_path <- glue('./Formatted_Q/{site}_dv.csv')
  write.csv(q_full, q_path)
  } # End of if statement
} # End of function

# Apply formatting function to all data files for all sites
# lapply(run.list, format_Q) # Comment after running once to avoid re-formatting

# PAIR WITH NLDAS PRECIP DATA -----------------------------------------------------
# Calculated area-weighted P using climrods python tool:
area_weighted_files <- list.files(path = './NLDAS_climrods/weighted_P', pattern = '.csv')

pair_P <- function(x){
  site <- sites[x]
  
  q_path <- glue('Q_{site}')
  q_file <- list.files(path = getwd(), pattern = q_path)
  type <- substr(q_file, 12, 13)
  
  nldas_file <- area_weighted_files[substr(area_weighted_files, 24, 31) == site]
  
  nldas_path <- glue('./NLDAS_climrods/weighted_P/{nldas_file}')
  nldas_P <- read.csv(nldas_path)
  nldas_P$DateTime <- ymd_hms(nldas_P$DateTime)
  nldas_P <- nldas_P[complete.cases(nldas_P),]
  colnames(nldas_P)[3] <- 'total_precipitation'
  
  Q_path <- list.files(paste0(getwd(), '/Formatted_Q/'), pattern = site)
  Q_path <- glue('./Formatted_Q/{Q_path}')
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
