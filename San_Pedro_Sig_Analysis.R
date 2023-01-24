# Analyze hydrologic signature results for the San Pedro River Watershed
# library(dataRetrieval)
library(tidyverse)
# library(glue)
library(lubridate)
library(data.table)

setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River')
monsoon_files <- list.files(path = './Monsoon_Data/Results/', pattern = '.csv')
monsoon_files <- paste0('./Monsoon_Data/Results/', monsoon_files)
full_files <- list.files(path = './QP_Data/Results/', pattern = '.csv')
full_files <- paste0('./QP_Data/Results/', full_files)

# sites_monsoon <- substr(monsoon_files, 16, 23)
# sites_full <- substr(full_files, 11, 18)
# sites_monsoon == sites_full
# sites <-  sites_monsoon
# rm(sites_monsoon, sites_full)


monsoon_sigs <- lapply(monsoon_files, read.csv)
monsoon_sigs <- rbindlist(monsoon_sigs)

full_sigs <- lapply(full_files, read.csv)
full_sigs <- rbindlist(full_sigs)

setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River/Formatted_Q')
file.list <- list.files(path = getwd(), pattern = '.csv')

x <- 1
data <- read.csv(file.list[x])
site_info <- readNWISsite('09470500')
da <- site_info$drain_area_va #mi2

da_m2 <- da*1000000

cfs <- 17525
cms <- cfs*0.0283168
ms <- cms/da_m2

ms*3600
