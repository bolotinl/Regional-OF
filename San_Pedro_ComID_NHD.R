## Call packages
x <- c("sf", "tidyverse", "nhdplusTools", "beepr", "dataRetrieval")
lapply(x, require, character.only = TRUE)
rm(x)
library(grid)
library(cowplot)

setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River')

## Bring in site list
site_info <- read.csv('Q_site_info.csv')

## Bring in location data
sites <- unique(zeroPad(site_info$site_no, 8))
meta <- readNWISsite(siteNumbers = sites)

## Subset for important info
names(meta)
meta <- meta %>%
  dplyr::select(c("site_no", "dec_lat_va", "dec_long_va", "huc_cd"))

## Format columns
meta$site_no <- paste0("USGS-", meta$site_no)

## Prepare a data frame where we will put the ComID's
sites <- factor(meta$site_no)
sites <- levels(sites)%>%
  as.data.frame()

sites$COMID <- "" # leave blank for now, this is what we are going to populate
colnames(sites)[1] <- "GAGE_ID"

#--------------------------------------------
## Assign NHD ComID using USGS SiteID
#--------------------------------------------

## Create function
findCOMID_USGS_ID <- function(x){ # x = USGS SiteID
  nldi_nwis <- list(featureSource = "nwissite", featureID = x)
  (sites$COMID[which(sites$GAGE_ID == x )] <<- discover_nhdplus_id(nldi_feature = nldi_nwis))
}
lapply(meta$site_no, findCOMID_USGS_ID)
beep() # Notifies us when the function above is done running with a sound


## Save data files
# As .csv file:
write.csv(sites, "./San_Pedro_COMIDs.csv")

#--------------------------------------------
## Pair NHD attributes
#--------------------------------------------


## BARREN LAND -------------------
nlcd <- read.table('../../../../../My Drive/Overland Flow MS/Data/Catchment Attribute Data/Land Cover/NLCD08_ACC_CONUS.TXT',
                   sep = ',', header = TRUE)
nlcd <- nlcd %>%
  dplyr::select(c(COMID, ACC_NLCD08_31)) %>%
  filter(COMID %in% sites$COMID) 

nlcd <- merge(sites, nlcd, by = 'COMID')
nlcd$GAGE_ID <- as.numeric(substr(nlcd$GAGE_ID, 6, 13))
colnames(nlcd) <- c('COMID', 'site', 'Barren Land')

sigs <- read.csv('./Signature Data/signature_df.csv')
sigs <- sigs %>%
  filter(OF_error_str == 'Warning: Ignoring NaNs in streamflow data. ')

barren_land <- merge(sigs, nlcd, by = 'site')

# Monsoon IE Effect ----
sd_mons_IE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_effect <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_IE_effect_cor)

# Monsoon SE effect ----
sd_mons_SE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_effect <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_SE_effect_cor)

# Monsoon IE thresh ----
sd_mons_IE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_thresh <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_IE_thresh_cor)

# Monsoon SE thresh ----
sd_mons_SE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_thresh <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
sd_mons_Storage_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_Storage_thresh <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
sd_mons_R_Pvol_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pvol_RC <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
sd_mons_R_Pint_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pint_RC <- ggplot(barren_land, aes(y = `Barren Land`, x = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_R_Pint_RC_cor)

#  IE Effect ----
sd_IE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_effect <- ggplot(barren_land, aes(y = `Barren Land`, x = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(y = 'Barren Land', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_IE_effect_cor)

#  SE effect ----
sd_SE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_effect <- ggplot(barren_land, aes(y = `Barren Land`, x = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Barren Land', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_SE_effect_cor)

#  IE thresh ----
sd_IE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_thresh <- ggplot(barren_land, aes(y = `Barren Land`, x = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Barren Land', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_IE_thresh_cor)

#  SE thresh ----
sd_SE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_thresh <- ggplot(barren_land, aes(y = `Barren Land`, x = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Barren Land', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_SE_thresh_cor)

#  Storage thresh ----
sd_Storage_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_Storage_thresh <- ggplot(barren_land, aes(y = `Barren Land`, x = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Barren Land', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_Storage_thresh_cor)

#  R Pvol ----
sd_R_Pvol_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pvol_RC <- ggplot(barren_land, aes(y = `Barren Land`, x = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Barren Land', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_R_Pvol_RC_cor)

#  R Pint ----
sd_R_Pint_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.8, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pint_RC <- ggplot(barren_land, aes(y = `Barren Land`, x = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Barren Land', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_R_Pint_RC_cor)

sd_IE_grid <- plot_grid(sd_IE_effect, sd_mons_IE_effect,
                        sd_IE_thresh, sd_mons_IE_thresh,
                        sd_R_Pint_RC, sd_mons_R_Pint_RC,
                        ncol = 2)
sd_IE_grid

sd_SE_grid <- plot_grid(sd_SE_effect, sd_mons_SE_effect,
                        sd_SE_thresh, sd_mons_SE_thresh,
                        sd_R_Pvol_RC, sd_mons_R_Pvol_RC,
                        sd_Storage_thresh, sd_mons_Storage_thresh,
                        ncol = 2)
sd_SE_grid



## STREAM DENSITY --------------------------
density <- read.table('../Catchment Attribute Data/STREAM_DENSITY_CONUS.TXT', sep = ',', header = TRUE)
density <- density %>%
  filter(COMID %in% sites$COMID) %>%
  dplyr::select(c(COMID, ACC_STRM_DENS))
colnames(density) <- c('COMID', 'Stream Density')

density <- merge(sites, density, by = 'COMID')
density$GAGE_ID <- as.numeric(substr(density$GAGE_ID, 6, 13))

density <- merge(density, sigs, by.x = 'GAGE_ID', by.y = 'site')

# Monsoon IE Effect ----
sd_mons_IE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_effect <- ggplot(density, aes(y = `Stream Density`, x = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_IE_effect_cor)

# Monsoon SE effect ----
sd_mons_SE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_effect <- ggplot(density, aes(y = `Stream Density`, x = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_SE_effect_cor)

# Monsoon IE thresh ----
sd_mons_IE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_thresh <- ggplot(density, aes(y = `Stream Density`, x = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_IE_thresh_cor)

# Monsoon SE thresh ----
sd_mons_SE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_thresh <- ggplot(density, aes(y = `Stream Density`, x = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
sd_mons_Storage_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_Storage_thresh <- ggplot(density, aes(y = `Stream Density`, x = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
sd_mons_R_Pvol_RC_cor <- cor.test(y =density$`Stream Density`, x =density$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pvol_RC <- ggplot(density, aes(y = `Stream Density`, x = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
sd_mons_R_Pint_RC_cor <- cor.test(y =density$`Stream Density`, x =density$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pint_RC <- ggplot(density, aes(y = `Stream Density`, x = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_mons_R_Pint_RC_cor)

#  IE Effect ----
sd_IE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_effect <- ggplot(density, aes(y = `Stream Density`, x = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(y = 'Stream Density', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_IE_effect_cor)

#  SE effect ----
sd_SE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_effect <- ggplot(density, aes(y = `Stream Density`, x = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream Density', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_SE_effect_cor)

#  IE thresh ----
sd_IE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_thresh <- ggplot(density, aes(y = `Stream Density`, x = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream Density', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_IE_thresh_cor)

#  SE thresh ----
sd_SE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_thresh <- ggplot(density, aes(y = `Stream Density`, x = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream Density', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_SE_thresh_cor)

#  Storage thresh ----
sd_Storage_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_Storage_thresh <- ggplot(density, aes(y = `Stream Density`, x = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream Density', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_Storage_thresh_cor)

#  R Pvol ----
sd_R_Pvol_RC_cor <- cor.test(y =density$`Stream Density`, x =density$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pvol_RC <- ggplot(density, aes(y = `Stream Density`, x = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream Density', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_R_Pvol_RC_cor)

#  R Pint ----
sd_R_Pint_RC_cor <- cor.test(y =density$`Stream Density`, x =density$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pint_RC <- ggplot(density, aes(y = `Stream Density`, x = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream Density', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(sd_R_Pint_RC_cor)

sd_IE_grid <- plot_grid(sd_IE_effect, sd_mons_IE_effect,
                        sd_IE_thresh, sd_mons_IE_thresh,
                        sd_R_Pint_RC, sd_mons_R_Pint_RC,
                        ncol = 2)
sd_IE_grid

sd_SE_grid <- plot_grid(sd_SE_effect, sd_mons_SE_effect,
                        sd_SE_thresh, sd_mons_SE_thresh,
                        sd_R_Pvol_RC, sd_mons_R_Pvol_RC,
                        sd_Storage_thresh, sd_mons_Storage_thresh,
                        ncol = 2)
sd_SE_grid
