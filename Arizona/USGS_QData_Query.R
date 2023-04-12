# Download and format data for a specified region of watersheds
# Code author: L Bolotin
library(dataRetrieval) # USGS data package
library(tidyverse) # Data handling
library(glue) # Put together literal strings
library(lubridate) # Handle datetimes

# Set the path for where you want to save your data
setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/Arizona')

## Query data --------------------------------------------------------------
# You can identify/select sites by huc 8 basin (e.g. Upper & Lower San Pedro)
# site_query <- whatNWISsites(parameterCd = '00060', # Specifies we want discharge
#                               huc = c('15050202', '15050203'))

# Or by state (e.g. Arizona)
site_query <- whatNWISsites(parameterCd = '00060', 
                            stateCd = 'AZ')

# Filter for streams only (as opposed to canals, ditches, etc.)
site_query <- site_query %>% filter(site_tp_cd == 'ST') 
site_query <- site_query %>% filter(!grepl('WASTEWAY|DIVERSION|CANAL|PUMP', station_nm))
sites <- unique(site_query$site_no)

# See what kind of data is available at these sites
data_query <- whatNWISdata(siteNumbers = sites)
data_query <- data_query %>% filter(parm_cd == '00060') # discharge only
data_query <- data_query %>% filter(data_type_cd == c('dv', 'uv')) # logger data only, not field observations, etc.

# Filter for sites that will have a decent amount of overlapping data with NLDAS precip data (1980-current)
data_query <- data_query %>% filter(end_date > '1985-01-01') 
data_query$period_of_record <- as.numeric(data_query$end_date - data_query$begin_date)
data_query <- data_query %>% filter(period_of_record >= 1825) # Five years of data

site_query <- site_query %>% filter(site_no %in% data_query$site_no)
sites <- unique(site_query$site_no)

# Filter for small basins (< 1000 km^2 or 386 mi^2)
site_metadata <- readNWISsite(sites)
site_metadata <- site_metadata %>% filter(drain_area_va <= 386)
sites <- unique(site_metadata$site_no)
site_query <- site_query %>% filter(site_no %in% sites)

# Output site metadata (including lat/long)
write.csv(site_query, './Q_site_info.csv')

# Split into smaller data frames for the purpose of downloading the NLDAS precip data later
site_query1 <- site_query[1:10,]
write.csv(site_query1, './Q_site_info_1.csv')
site_query2 <- site_query[11:20,]
write.csv(site_query2, './Q_site_info_2.csv')
site_query3 <- site_query[21:35,]
write.csv(site_query3, './Q_site_info_3.csv')
site_query4 <- site_query[36:59,]
write.csv(site_query4, './Q_site_info_4.csv')
# rm(site_query1, site_query2, site_query3, site_query4)

## Download data --------------------------------------------------------------
download_Q <- function(x){
  
site <- sites[x]
data <- data_query %>% filter(site_no == site)
types <- data$data_type_cd

# If sub-daily data is available, download it. If not, download daily data.
if ('uv' %in% types) {
  start_date <- data_query$begin_date[data_query$site_no == site & data_query$data_type_cd == 'uv'][1]
  q <- readNWISuv(site, '00060', start_date, '') # Grabs full period of available data after NLDAS becomes available
} else {
  start_date <- data_query$begin_date[data_query$site_no == site & data_query$data_type_cd == 'dv'][1]
  q <- readNWISdv(site, '00060', start_date, '') 
}

type <- ifelse('uv' %in% types, 'uv', 'dv')

q_path <- glue('./Streamflow Data/Q_{site}_{type}.csv')
write.csv(q, q_path)
}

run.list <- seq(1, length(sites))

# Apply download function to all sites:
# lapply(run.list, download_Q) # Comment after running once to avoid re-downloading

## Format Data ----------------------------------------------------------------
format_Q <- function(x) {

# Set path to where the raw streamflow data was saved
setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/Arizona/Streamflow Data')
site <- sites[x]
q_path <- glue('Q_{site}')
q_file <- list.files(path = '/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/Arizona/Streamflow Data', pattern = q_path)

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

  q_path <- glue('../Formatted Streamflow Data/{site}_uv.csv')
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
  # if (is.na(da)) {
  #   watershed_areas <- read.csv('watershed_areas.csv')
  #   da <- watershed_areas$AREA[zeroPad(watershed_areas$GAGE_ID,8) == site] # sq m
  #   da <- da * 3.861022e-7 #sq mi
  # }
  drainage_area <- da*27878400 # sq mi to sq ft
  
  q_full$Q_ft_s <- q_full$QObs_cfs/drainage_area 
  q_full$Q_mm_d <- q_full$Q_ft_s*304.8*86400

  q_path <- glue('../Formatted Streamflow Data/{site}_dv.csv')
  write.csv(q_full, q_path)
  } # End of if statement
} # End of function

# Apply formatting function to all data files for all sites
lapply(run.list, format_Q) # Comment after running once to avoid re-formatting
