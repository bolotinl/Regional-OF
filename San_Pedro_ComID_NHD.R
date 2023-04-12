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


sigs <- read.csv('./Signature Data/area_weighted_NLDAS/signature_df.csv')
sigs <- sigs %>%
  filter(OF_error_str == 'Warning: Ignoring NaNs in streamflow data. ')
sigs <- sigs %>%
  filter(site != 9470820)
## BARREN LAND -------------------
nlcd <- read.table('../../../../../My Drive/Overland Flow MS/Data/Catchment Attribute Data/Land Cover/NLCD08_ACC_CONUS.TXT',
                   sep = ',', header = TRUE)
nlcd <- nlcd %>%
  dplyr::select(c(COMID, ACC_NLCD08_31)) %>%
  filter(COMID %in% sites$COMID) 

nlcd <- merge(sites, nlcd, by = 'COMID')
nlcd$GAGE_ID <- as.numeric(substr(nlcd$GAGE_ID, 6, 13))
colnames(nlcd) <- c('COMID', 'site', 'Barren Land')

barren_land <- merge(sigs, nlcd, by = 'site')

# Monsoon IE Effect ----
sd_mons_IE_effect_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_effect <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_IE_effect_mons.png', plot = sd_mons_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_IE_effect_cor)

# Monsoon SE effect ----
sd_mons_SE_effect_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_effect <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_SE_effect_mons.png', plot = sd_mons_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_SE_effect_cor)

# Monsoon IE thresh ----
sd_mons_IE_thresh_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_thresh <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_IE_thresh_mons.png', plot = sd_mons_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_IE_thresh_cor)

# Monsoon SE thresh ----
sd_mons_SE_thresh_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_thresh <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_SE_thresh_mons.png', plot = sd_mons_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
sd_mons_Storage_thresh_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_Storage_thresh <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = 'Barren Land', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_Storage_thresh_mons.png', plot = sd_mons_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
sd_mons_R_Pvol_RC_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pvol_RC <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_R_Pvol_RC_mons.png', plot = sd_mons_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
sd_mons_R_Pint_RC_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pint_RC <- ggplot(barren_land, aes(x = `Barren Land`, y = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = 'Barren Land', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_R_Pint_RC_mons.png', plot = sd_mons_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_R_Pint_RC_cor)

#  IE Effect ----
sd_IE_effect_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_effect <- ggplot(barren_land, aes(x = `Barren Land`, y = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(x = '', y = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_IE_effect.png', plot = sd_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_IE_effect_cor)

#  SE effect ----
sd_SE_effect_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_effect <- ggplot(barren_land, aes(x = `Barren Land`, y = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_SE_effect.png', plot = sd_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_SE_effect_cor)

#  IE thresh ----
sd_IE_thresh_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_thresh <- ggplot(barren_land, aes(x = `Barren Land`, y = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_IE_thresh.png', plot = sd_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_IE_thresh_cor)

#  SE thresh ----
sd_SE_thresh_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_thresh <- ggplot(barren_land, aes(x = `Barren Land`, y = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Barren Land', y = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_SE_thresh.png', plot = sd_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_SE_thresh_cor)

#  Storage thresh ----
sd_Storage_thresh_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_Storage_thresh <- ggplot(barren_land, aes(x = `Barren Land`, y = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Barren Land', y = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_Storage_thresh.png', plot = sd_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_Storage_thresh_cor)

#  R Pvol ----
sd_R_Pvol_RC_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pvol_RC <- ggplot(barren_land, aes(x = `Barren Land`, y = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Barren Land', y = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_R_Pvol_RC.png', plot = sd_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_R_Pvol_RC_cor)

#  R Pint ----
sd_R_Pint_RC_cor <- cor.test(x =barren_land$`Barren Land`, y =barren_land$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.75, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.9, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pint_RC <- ggplot(barren_land, aes(x = `Barren Land`, y = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Barren Land', y = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_R_Pint_RC.png', plot = sd_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_R_Pint_RC_cor)

sd_IE_grid <- plot_grid(sd_IE_effect, sd_mons_IE_effect,
                        sd_IE_thresh, sd_mons_IE_thresh,
                        sd_R_Pint_RC, sd_mons_R_Pint_RC,
                        ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_IE_grid.png', plot = sd_IE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
sd_IE_grid

sd_SE_grid <- plot_grid(sd_SE_effect, sd_mons_SE_effect,
                        sd_SE_thresh, sd_mons_SE_thresh,
                        sd_R_Pvol_RC, sd_mons_R_Pvol_RC,
                        sd_Storage_thresh, sd_mons_Storage_thresh,
                        ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_BarrenLand_SE_grid.png', plot = sd_SE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
sd_SE_grid

SP_df <- barren_land
rm(sd_IE_effect, sd_Storage_thresh, sd_SE_thresh, sd_SE_grid, sd_SE_effect, sd_R_Pvol_RC, sd_R_Pint_RC,
   sd_mons_Storage_thresh, sd_mons_SE_thresh, sd_mons_SE_effect, sd_mons_R_Pvol_RC, sd_mons_R_Pint_RC,
   sd_mons_IE_thresh, sd_mons_IE_effect, sd_IE_thresh, sd_mons_IE_effect, sd_IE_grid,nlcd, barren_land)

## STREAM DENSITY --------------------------
density <- read.table('../Catchment Attribute Data/STREAM_DENSITY_CONUS.TXT', sep = ',', header = TRUE)
density <- density %>%
  filter(COMID %in% sites$COMID) %>%
  dplyr::select(c(COMID, ACC_STRM_DENS))
colnames(density) <- c('COMID', 'Stream Density')

density <- merge(sites, density, by = 'COMID')
density$GAGE_ID <- as.numeric(substr(density$GAGE_ID, 6, 13))
SP_df <- merge(SP_df, density, by.x = c('site', 'COMID'), by.y = c('GAGE_ID', 'COMID'))

density <- merge(density, sigs, by.x = 'GAGE_ID', by.y = 'site')

# Monsoon IE Effect ----
sd_mons_IE_effect_cor <- cor.test(x =density$`Stream Density`, y =density$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_effect <- ggplot(density, aes(x = `Stream Density`, y = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_IE_effect_mons.png', plot = sd_mons_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_IE_effect_cor)

# Monsoon SE effect ----
sd_mons_SE_effect_cor <- cor.test(x =density$`Stream Density`, y =density$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_effect <- ggplot(density, aes(x = `Stream Density`, y = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_SE_effect_mons.png', plot = sd_mons_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_SE_effect_cor)

# Monsoon IE thresh ----
sd_mons_IE_thresh_cor <- cor.test(x =density$`Stream Density`, y =density$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_IE_thresh <- ggplot(density, aes(x = `Stream Density`, y = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_IE_thresh_mons.png', plot = sd_mons_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_IE_thresh_cor)

# Monsoon SE thresh ----
sd_mons_SE_thresh_cor <- cor.test(x =density$`Stream Density`, y =density$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_SE_thresh <- ggplot(density, aes(x = `Stream Density`, y = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_SE_thresh_mons.png', plot = sd_mons_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
sd_mons_Storage_thresh_cor <- cor.test(x =density$`Stream Density`, y =density$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_Storage_thresh <- ggplot(density, aes(x = `Stream Density`, y = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Stream Density')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_Storage_thresh_mons.png', plot = sd_mons_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
sd_mons_R_Pvol_RC_cor <- cor.test(x =density$`Stream Density`, y =density$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pvol_RC <- ggplot(density, aes(x = `Stream Density`, y = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_R_Pvol_RC_mons.png', plot = sd_mons_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
sd_mons_R_Pint_RC_cor <- cor.test(x =density$`Stream Density`, y =density$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_mons_R_Pint_RC <- ggplot(density, aes(x = `Stream Density`, y = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_R_Pint_RC_mons.png', plot = sd_mons_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(sd_mons_R_Pint_RC_cor)

#  IE Effect ----
sd_IE_effect_cor <- cor.test(x =density$`Stream Density`, y =density$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_effect <- ggplot(density, aes(x = `Stream Density`, y = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(x = '', y = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_IE_effect.png', plot = sd_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_IE_effect_cor)

#  SE effect ----
sd_SE_effect_cor <- cor.test(x =density$`Stream Density`, y =density$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_effect <- ggplot(density, aes(x = `Stream Density`, y = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_SE_effect.png', plot = sd_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_SE_effect_cor)

#  IE thresh ----
sd_IE_thresh_cor <- cor.test(x =density$`Stream Density`, y =density$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_IE_thresh <- ggplot(density, aes(x = `Stream Density`, y = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_IE_thresh.png', plot = sd_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_IE_thresh_cor)

#  SE thresh ----
sd_SE_thresh_cor <- cor.test(x =density$`Stream Density`, y =density$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_SE_thresh <- ggplot(density, aes(x = `Stream Density`, y = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_SE_thresh.png', plot = sd_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_SE_thresh_cor)

#  Storage thresh ----
sd_Storage_thresh_cor <- cor.test(x =density$`Stream Density`, y =density$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_Storage_thresh <- ggplot(density, aes(x = `Stream Density`, y = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Stream Density', y = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_Storage_thresh.png', plot = sd_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_Storage_thresh_cor)

#  R Pvol ----
sd_R_Pvol_RC_cor <- cor.test(x =density$`Stream Density`, y =density$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pvol_RC <- ggplot(density, aes(x = `Stream Density`, y = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_R_Pvol_RC.png', plot = sd_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_R_Pvol_RC_cor)

#  R Pint ----
sd_R_Pint_RC_cor <- cor.test(x =density$`Stream Density`, y =density$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(sd_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(sd_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
sd_R_Pint_RC <- ggplot(density, aes(x = `Stream Density`, y = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Stream Density', y = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_R_Pint_RC.png', plot = sd_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(sd_R_Pint_RC_cor)

sd_IE_grid <- plot_grid(sd_IE_effect, sd_mons_IE_effect,
                        sd_IE_thresh, sd_mons_IE_thresh,
                        sd_R_Pint_RC, sd_mons_R_Pint_RC,
                        ncol = 2)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_IE_grid.png', plot = sd_IE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
sd_IE_grid

sd_SE_grid <- plot_grid(sd_SE_effect, sd_mons_SE_effect,
                        sd_SE_thresh, sd_mons_SE_thresh,
                        sd_R_Pvol_RC, sd_mons_R_Pvol_RC,
                        sd_Storage_thresh, sd_mons_Storage_thresh,
                        ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_StreamDensity_SE_grid.png', plot = sd_SE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
sd_SE_grid

rm(sd_IE_effect, sd_Storage_thresh, sd_SE_thresh, sd_SE_grid, sd_SE_effect, sd_R_Pvol_RC, sd_R_Pint_RC,
   sd_mons_Storage_thresh, sd_mons_SE_thresh, sd_mons_SE_effect, sd_mons_R_Pvol_RC, sd_mons_R_Pint_RC,
   sd_mons_IE_thresh, sd_mons_IE_effect, sd_IE_thresh, sd_mons_IE_effect, sd_IE_grid, density)

## KSAT ------------------------------
ksat <- read.csv('/Users/laurenbolotin/Desktop/san_pedro_ksat.csv')
ksat <- ksat[,1:7]
ksat[,3:7] <- ksat[,3:7]/100

SP_df <- merge(SP_df, ksat, by.x = 'site', by.y = 'GAGE_ID')
SP_df <- SP_df %>% select(-c(X_count, X_sum))
colnames(SP_df)[32:35] <- c('ksat_mean', 'ksat_median','ksat_min', 'ksat_max')
ksat <- merge(sigs, ksat, by.x = 'site', by.y = 'GAGE_ID', all.x = TRUE, all.y = FALSE)


# Monsoon IE Effect ----
ksat_mons_IE_effect_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_IE_effect <- ggplot(ksat, aes(x = X_mean, y = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_IE_effect_mons.png', plot = ksat_mons_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_IE_effect_cor)

# Monsoon SE effect ----
ksat_mons_SE_effect_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_SE_effect <- ggplot(ksat, aes(x = X_mean, y = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_SE_effect_mons.png', plot = ksat_mons_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_SE_effect_cor)

# Monsoon IE thresh ----
ksat_mons_IE_thresh_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_IE_thresh <- ggplot(ksat, aes(x = X_mean, y = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_IE_thresh_mons.png', plot = ksat_mons_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_IE_thresh_cor)

# Monsoon SE thresh ----
ksat_mons_SE_thresh_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_SE_thresh <- ggplot(ksat, aes(x = X_mean, y = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_SE_thresh_mons.png', plot = ksat_mons_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
ksat_mons_Storage_thresh_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_Storage_thresh <- ggplot(ksat, aes(x = X_mean, y = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = 'Ksat', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_Storage_thresh_mons.png', plot = ksat_mons_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
ksat_mons_R_Pvol_RC_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_R_Pvol_RC <- ggplot(ksat, aes(x = X_mean, y = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_R_Pvol_RC_mons.png', plot = ksat_mons_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
ksat_mons_R_Pint_RC_cor <- cor.test(x =ksat$X_mean, y =ksat$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_R_Pint_RC <- ggplot(ksat, aes(x = X_mean, y = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = 'Ksat', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_R_Pint_RC_mons.png', plot = ksat_mons_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_mons_R_Pint_RC_cor)

#  IE Effect ----
ksat_IE_effect_cor <- cor.test(x =ksat$X_mean, y =ksat$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_IE_effect <- ggplot(ksat, aes(x = X_mean, y = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(x = '', y = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_IE_effect.png', plot = ksat_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_IE_effect_cor)

#  SE effect ----
ksat_SE_effect_cor <- cor.test(x =ksat$X_mean, y =ksat$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_SE_effect <- ggplot(ksat, aes(x = X_mean, y = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_SE_effect.png', plot = ksat_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_SE_effect_cor)

#  IE thresh ----
ksat_IE_thresh_cor <- cor.test(x =ksat$X_mean, y =ksat$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_IE_thresh <- ggplot(ksat, aes(x = X_mean, y = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_IE_thresh.png', plot = ksat_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_IE_thresh_cor)

#  SE thresh ----
ksat_SE_thresh_cor <- cor.test(x =ksat$X_mean, y =ksat$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_SE_thresh <- ggplot(ksat, aes(x = X_mean, y = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_SE_thresh.png', plot = ksat_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_SE_thresh_cor)

#  Storage thresh ----
ksat_Storage_thresh_cor <- cor.test(x =ksat$X_mean, y =ksat$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_Storage_thresh <- ggplot(ksat, aes(x = X_mean, y = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Ksat', y = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_Storage_thresh.png', plot = ksat_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_Storage_thresh_cor)

#  R Pvol ----
ksat_R_Pvol_RC_cor <- cor.test(x =ksat$X_mean, y =ksat$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_R_Pvol_RC <- ggplot(ksat, aes(x = X_mean, y = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_R_Pvol_RC.png', plot = ksat_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_R_Pvol_RC_cor)

#  R Pint ----
ksat_R_Pint_RC_cor <- cor.test(x =ksat$X_mean, y =ksat$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_R_Pint_RC <- ggplot(ksat, aes(x = X_mean, y = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Ksat', y = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_R_Pint_RC.png', plot = ksat_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(ksat_R_Pint_RC_cor)

ksat_IE_grid <- plot_grid(ksat_IE_effect, ksat_mons_IE_effect,
                        ksat_IE_thresh, ksat_mons_IE_thresh,
                        ksat_R_Pint_RC, ksat_mons_R_Pint_RC,
                        ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_IE_grid.png', plot = ksat_IE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')

ksat_IE_grid

ksat_SE_grid <- plot_grid(ksat_SE_effect, ksat_mons_SE_effect,
                        ksat_SE_thresh, ksat_mons_SE_thresh,
                        ksat_R_Pvol_RC, ksat_mons_R_Pvol_RC,
                        ksat_Storage_thresh, ksat_mons_Storage_thresh,
                        ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Ksat_SE_grid.png', plot = ksat_SE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
ksat_SE_grid

rm(ksat, ksat_IE_grid, ksat_SE_grid, ksat_IE_effect, ksat_IE_thresh, ksat_SE_effect, ksat_SE_thresh,
   ksat_Storage_thresh, ksat_mons_IE_effect, ksat_mons_IE_thresh, ksat_mons_SE_thresh, ksat_mons_SE_effect,
   ksat_mons_Storage_thresh, ksat_mons_R_Pvol_RC, ksat_mons_R_Pint_RC, ksat_R_Pint_RC, ksat_R_Pvol_RC)



# DEPTH TO BEDROCK ---------------------------
DTBR <- read.csv('./San_Pedro_DTBR.csv')

SP_df <- merge(SP_df, DTBR, by.x = 'site', by.y = 'GAGE_ID')
SP_df <- SP_df %>% select(-c(X_count, X_sum))
colnames(SP_df)[36:39] <- c('DTBR_mean', 'DTBR_median', 'DTBR_min', 'DTBR_max')
DTBR <- merge(sigs, DTBR, by.x = 'site', by.y = 'GAGE_ID', all.x = TRUE, all.y = FALSE)


# Monsoon IE Effect ----
DTBR_mons_IE_effect_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_IE_effect <- ggplot(DTBR, aes(x = X_mean, y = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = '', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_IE_effect_mons.png', plot = DTBR_mons_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_mons_IE_effect_cor)

# Monsoon SE effect ----
DTBR_mons_SE_effect_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_SE_effect <- ggplot(DTBR, aes(x = X_mean, y = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_SE_effect_mons.png', plot = DTBR_mons_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_mons_SE_effect_cor)

# Monsoon IE thresh ----
DTBR_mons_IE_thresh_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_IE_thresh <- ggplot(DTBR, aes(x = X_mean, y = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_IE_thresh_mons.png', plot = DTBR_mons_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_mons_IE_thresh_cor)

# Monsoon SE thresh ----
DTBR_mons_SE_thresh_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_SE_thresh <- ggplot(DTBR, aes(x = X_mean, y = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_SE_thresh_mons.png', plot = DTBR_mons_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(DTBR_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
DTBR_mons_Storage_thresh_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_Storage_thresh <- ggplot(DTBR, aes(x = X_mean, y = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Depth to Bedrock')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_Storage_thresh_mons.png', plot = DTBR_mons_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
DTBR_mons_R_Pvol_RC_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_R_Pvol_RC <- ggplot(DTBR, aes(x = X_mean, y = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_R_Pvol_RC_mons.png', plot = DTBR_mons_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
DTBR_mons_R_Pint_RC_cor <- cor.test(x =DTBR$X_mean, y =DTBR$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_mons_R_Pint_RC <- ggplot(DTBR, aes(x = X_mean, y = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_R_Pint_RC_mons.png', plot = DTBR_mons_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_mons_R_Pint_RC_cor)

#  IE Effect ----
DTBR_IE_effect_cor <- cor.test(x =DTBR$X_mean, y =DTBR$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_IE_effect <- ggplot(DTBR, aes(x = X_mean, y = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(x = '', y = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_IE_effect.png', plot = DTBR_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_IE_effect_cor)

#  SE effect ----
DTBR_SE_effect_cor <- cor.test(x =DTBR$X_mean, y =DTBR$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_SE_effect <- ggplot(DTBR, aes(x = X_mean, y = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_SE_effect.png', plot = DTBR_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_SE_effect_cor)

#  IE thresh ----
DTBR_IE_thresh_cor <- cor.test(x =DTBR$X_mean, y =DTBR$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_IE_thresh <- ggplot(DTBR, aes(x = X_mean, y = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_IE_thresh.png', plot = DTBR_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_IE_thresh_cor)

#  SE thresh ----
DTBR_SE_thresh_cor <- cor.test(x =DTBR$X_mean, y =DTBR$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_SE_thresh <- ggplot(DTBR, aes(x = X_mean, y = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_SE_thresh.png', plot = DTBR_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_SE_thresh_cor)

#  Storage thresh ----
DTBR_Storage_thresh_cor <- cor.test(x =DTBR$X_mean, y =DTBR$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_Storage_thresh <- ggplot(DTBR, aes(x = X_mean, y = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Depth to Bedrock', y = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_Storage_thresh.png', plot = DTBR_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_Storage_thresh_cor)

#  R Pvol ----
DTBR_R_Pvol_RC_cor <- cor.test(x =DTBR$X_mean, y =DTBR$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_R_Pvol_RC <- ggplot(DTBR, aes(x = X_mean, y = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_R_Pvol_RC.png', plot = DTBR_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_R_Pvol_RC_cor)

#  R Pint ----
DTBR_R_Pint_RC_cor <- cor.test(x =DTBR$X_mean, y =DTBR$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(DTBR_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(DTBR_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
DTBR_R_Pint_RC <- ggplot(DTBR, aes(x = X_mean, y = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Depth to Bedrock', y = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_R_Pint_RC.png', plot = DTBR_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(DTBR_R_Pint_RC_cor)

DTBR_IE_grid <- plot_grid(DTBR_IE_effect, DTBR_mons_IE_effect,
                          DTBR_IE_thresh, DTBR_mons_IE_thresh,
                          DTBR_R_Pint_RC, DTBR_mons_R_Pint_RC,
                          ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_IE_grid.png', plot = DTBR_IE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')

DTBR_IE_grid

DTBR_SE_grid <- plot_grid(DTBR_SE_effect, DTBR_mons_SE_effect,
                          DTBR_SE_thresh, DTBR_mons_SE_thresh,
                          DTBR_R_Pvol_RC, DTBR_mons_R_Pvol_RC,
                          DTBR_Storage_thresh, DTBR_mons_Storage_thresh,
                          ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_DTBR_SE_grid.png', plot = DTBR_SE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
DTBR_SE_grid

rm(DTBR, DTBR_IE_grid, DTBR_SE_grid, DTBR_IE_effect, DTBR_IE_thresh, DTBR_SE_effect, DTBR_SE_thresh,
   DTBR_Storage_thresh, DTBR_mons_IE_effect, DTBR_mons_IE_thresh, DTBR_mons_SE_thresh, DTBR_mons_SE_effect,
   DTBR_mons_Storage_thresh, DTBR_mons_R_Pvol_RC, DTBR_mons_R_Pint_RC, DTBR_R_Pint_RC, DTBR_R_Pvol_RC)


# PERMEABILITY ---------------------------
perm <- read.table('../Catchment Attribute Data/OLSON_ACC_CONUS.TXT', sep = ',', header = TRUE)
perm <- perm %>%
  filter(COMID %in% sites$COMID) %>%
  dplyr::select(c(COMID, ACC_OLSON_PERM))
colnames(perm) <- c('COMID', 'permeability')

SP_df <- merge(SP_df, perm, by = 'COMID')
perm <- merge(sites, perm, by = 'COMID')
perm$GAGE_ID <- as.numeric(substr(perm$GAGE_ID, 6, 13))

perm <- merge(perm, sigs, by.x = 'GAGE_ID', by.y = 'site')


# Monsoon IE Effect ----
perm_mons_IE_effect_cor <- cor.test(x =perm$permeability, y =perm$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_IE_effect <- ggplot(perm, aes(x = permeability, y = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_IE_effect_mons.png', plot = perm_mons_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_mons_IE_effect_cor)

# Monsoon SE effect ----
perm_mons_SE_effect_cor <- cor.test(x =perm$permeability, y =perm$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_SE_effect <- ggplot(perm, aes(x = permeability, y = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_SE_effect_mons.png', plot = perm_mons_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_mons_SE_effect_cor)

# Monsoon IE thresh ----
perm_mons_IE_thresh_cor <- cor.test(x =perm$permeability, y =perm$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_IE_thresh <- ggplot(perm, aes(x = permeability, y = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_IE_thresh_mons.png', plot = perm_mons_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_mons_IE_thresh_cor)

# Monsoon SE thresh ----
perm_mons_SE_thresh_cor <- cor.test(x =perm$permeability, y =perm$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_SE_thresh <- ggplot(perm, aes(x = permeability, y = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_SE_thresh_mons.png', plot = perm_mons_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
perm_mons_Storage_thresh_cor <- cor.test(x =perm$permeability, y =perm$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_Storage_thresh <- ggplot(perm, aes(x = permeability, y = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(x = 'Permeability', y = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_Storage_thresh_mons.png', plot = perm_mons_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
perm_mons_R_Pvol_RC_cor <- cor.test(x =perm$permeability, y =perm$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_R_Pvol_RC <- ggplot(perm, aes(x = permeability, y = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = '')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_R_Pvol_RC_mons.png', plot = perm_mons_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(perm_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
perm_mons_R_Pint_RC_cor <- cor.test(x =perm$permeability, y =perm$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_mons_R_Pint_RC <- ggplot(perm, aes(x = permeability, y = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Permeability')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_R_Pint_RC_mons.png', plot = perm_mons_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(perm_mons_R_Pint_RC_cor)

#  IE Effect ----
perm_IE_effect_cor <- cor.test(x =perm$permeability, y =perm$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_IE_effect <- ggplot(perm, aes(x = permeability, y = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(x = '', y = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_IE_effect.png', plot = perm_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_IE_effect_cor)

#  SE effect ----
perm_SE_effect_cor <- cor.test(x =perm$permeability, y =perm$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_SE_effect <- ggplot(perm, aes(x = permeability, y = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_SE_effect.png', plot = perm_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_SE_effect_cor)

#  IE thresh ----
perm_IE_thresh_cor <- cor.test(x =perm$permeability, y =perm$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_IE_thresh <- ggplot(perm, aes(x = permeability, y = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_IE_thresh.png', plot = perm_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_IE_thresh_cor)

#  SE thresh ----
perm_SE_thresh_cor <- cor.test(x =perm$permeability, y =perm$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_SE_thresh <- ggplot(perm, aes(x = permeability, y = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_SE_thresh.png', plot = perm_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_SE_thresh_cor)

#  Storage thresh ----
perm_Storage_thresh_cor <- cor.test(x =perm$permeability, y =perm$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_Storage_thresh <- ggplot(perm, aes(x = permeability, y = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Permeability', y = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_Storage_thresh.png', plot = perm_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_Storage_thresh_cor)

#  R Pvol ----
perm_R_Pvol_RC_cor <- cor.test(x =perm$permeability, y =perm$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_R_Pvol_RC <- ggplot(perm, aes(x = permeability, y = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = '', y = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_R_Pvol_RC.png', plot = perm_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_R_Pvol_RC_cor)

#  R Pint ----
perm_R_Pint_RC_cor <- cor.test(x =perm$permeability, y =perm$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(perm_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(perm_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
perm_R_Pint_RC <- ggplot(perm, aes(x = permeability, y = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(x = 'Permeability', y = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_R_Pint_RC.png', plot = perm_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(perm_R_Pint_RC_cor)

perm_IE_grid <- plot_grid(perm_IE_effect, perm_mons_IE_effect,
                        perm_IE_thresh, perm_mons_IE_thresh,
                        perm_R_Pint_RC, perm_mons_R_Pint_RC,
                        ncol = 2)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_IE_grid.png', plot = perm_IE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
perm_IE_grid

perm_SE_grid <- plot_grid(perm_SE_effect, perm_mons_SE_effect,
                        perm_SE_thresh, perm_mons_SE_thresh,
                        perm_R_Pvol_RC, perm_mons_R_Pvol_RC,
                        perm_Storage_thresh, perm_mons_Storage_thresh,
                        ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Permeability_SE_grid.png', plot = perm_SE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
perm_SE_grid

rm(perm_IE_effect, perm_Storage_thresh, perm_SE_thresh, perm_SE_grid, perm_SE_effect, perm_R_Pvol_RC, perm_R_Pint_RC,
   perm_mons_Storage_thresh, perm_mons_SE_thresh, perm_mons_SE_effect, perm_mons_R_Pvol_RC, perm_mons_R_Pint_RC,
   perm_mons_IE_thresh, perm_mons_IE_effect, perm_IE_thresh, perm_mons_IE_effect, perm_IE_grid, perm)



# IMPERVIOUSNESS ---------------------------
imp <- read.table('../Catchment Attribute Data/IMPV06_CONUS.txt', sep = ',', header = TRUE)
imp <- imp %>%
  filter(COMID %in% sites$COMID) %>%
  dplyr::select(c(COMID, ACC_IMPV06))
colnames(imp) <- c('COMID', 'Pct_Impervious')

SP_df <- merge(SP_df, imp, by = 'COMID')

imp <- merge(sites, imp, by = 'COMID')
imp$GAGE_ID <- as.numeric(substr(imp$GAGE_ID, 6, 13))

imp <- merge(imp, sigs, by.x = 'GAGE_ID', by.y = 'site')


# Monsoon IE Effect ----
imp_mons_IE_effect_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_IE_effect <- ggplot(imp, aes(y = Pct_Impervious, x = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_IE_effect_mons.png', plot = imp_mons_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_mons_IE_effect_cor)

# Monsoon SE effect ----
imp_mons_SE_effect_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_SE_effect <- ggplot(imp, aes(y = Pct_Impervious, x = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_SE_effect_mons.png', plot = imp_mons_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_mons_SE_effect_cor)

# Monsoon IE thresh ----
imp_mons_IE_thresh_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_IE_thresh <- ggplot(imp, aes(y = Pct_Impervious, x = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_IE_thresh_mons.png', plot = imp_mons_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_mons_IE_thresh_cor)

# Monsoon SE thresh ----
imp_mons_SE_thresh_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_SE_thresh <- ggplot(imp, aes(y = Pct_Impervious, x = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_SE_thresh_mons.png', plot = imp_mons_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
imp_mons_Storage_thresh_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_Storage_thresh <- ggplot(imp, aes(y = Pct_Impervious, x = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_Storage_thresh_mons.png', plot = imp_mons_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
imp_mons_R_Pvol_RC_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_R_Pvol_RC <- ggplot(imp, aes(y = Pct_Impervious, x = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_R_Pvol_RC_mons.png', plot = imp_mons_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(imp_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
imp_mons_R_Pint_RC_cor <- cor.test(y =imp$Pct_Impervious, x =imp$mons_R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_mons_R_Pint_RC <- ggplot(imp, aes(y = Pct_Impervious, x = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_R_Pint_RC_mons.png', plot = imp_mons_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')

rm(imp_mons_R_Pint_RC_cor)

#  IE Effect ----
imp_IE_effect_cor <- cor.test(y =imp$Pct_Impervious, x =imp$IE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_IE_effect <- ggplot(imp, aes(y = Pct_Impervious, x = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(y = '% Impervious', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_IE_effect.png', plot = imp_IE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_IE_effect_cor)

#  SE effect ----
imp_SE_effect_cor <- cor.test(y =imp$Pct_Impervious, x =imp$SE_effect, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_SE_effect <- ggplot(imp, aes(y = Pct_Impervious, x = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = '% Impervious', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_SE_effect.png', plot = imp_SE_effect, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_SE_effect_cor)

#  IE thresh ----
imp_IE_thresh_cor <- cor.test(y =imp$Pct_Impervious, x =imp$IE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_IE_thresh <- ggplot(imp, aes(y = Pct_Impervious, x = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = '% Impervious', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_IE_thresh.png', plot = imp_IE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_IE_thresh_cor)

#  SE thresh ----
imp_SE_thresh_cor <- cor.test(y =imp$Pct_Impervious, x =imp$SE_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_SE_thresh <- ggplot(imp, aes(y = Pct_Impervious, x = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = '% Impervious', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_SE_thresh.png', plot = imp_SE_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_SE_thresh_cor)

#  Storage thresh ----
imp_Storage_thresh_cor <- cor.test(y =imp$Pct_Impervious, x =imp$Storage_thresh, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_Storage_thresh <- ggplot(imp, aes(y = Pct_Impervious, x = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = '% Impervious', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_Storage_thresh.png', plot = imp_Storage_thresh, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_Storage_thresh_cor)

#  R Pvol ----
imp_R_Pvol_RC_cor <- cor.test(y =imp$Pct_Impervious, x =imp$R_Pvol_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_R_Pvol_RC <- ggplot(imp, aes(y = Pct_Impervious, x = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = '% Impervious', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_R_Pvol_RC.png', plot = imp_R_Pvol_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_R_Pvol_RC_cor)

#  R Pint ----
imp_R_Pint_RC_cor <- cor.test(y =imp$Pct_Impervious, x =imp$R_Pint_RC, method = 'spearman', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(imp_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(imp_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
imp_R_Pint_RC <- ggplot(imp, aes(y = Pct_Impervious, x = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = '% Impervious', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_R_Pint_RC.png', plot = imp_R_Pint_RC, width = 3.5, height = 3.5, units = c('in'), dpi = 300, bg = 'white')
rm(imp_R_Pint_RC_cor)

imp_IE_grid <- plot_grid(imp_IE_effect, imp_mons_IE_effect,
                          imp_IE_thresh, imp_mons_IE_thresh,
                          imp_R_Pint_RC, imp_mons_R_Pint_RC,
                          ncol = 2)

ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_IE_grid.png', plot = imp_IE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
imp_IE_grid

imp_SE_grid <- plot_grid(imp_SE_effect, imp_mons_SE_effect,
                          imp_SE_thresh, imp_mons_SE_thresh,
                          imp_R_Pvol_RC, imp_mons_R_Pvol_RC,
                          imp_Storage_thresh, imp_mons_Storage_thresh,
                          ncol = 2)
ggsave('../Figures/Correlation Plots/San Pedro/San_Pedro_Pct_Impervious_SE_grid.png', plot = imp_SE_grid, width = 6, height = 7.5, units = c('in'), dpi = 300, bg = 'white')
imp_SE_grid

rm(imp_IE_effect, imp_Storage_thresh, imp_SE_thresh, imp_SE_grid, imp_SE_effect, imp_R_Pvol_RC, imp_R_Pint_RC,
   imp_mons_Storage_thresh, imp_mons_SE_thresh, imp_mons_SE_effect, imp_mons_R_Pvol_RC, imp_mons_R_Pint_RC,
   imp_mons_IE_thresh, imp_mons_IE_effect, imp_IE_thresh, imp_mons_IE_effect, imp_IE_grid, imp)

meta$site_no <- as.numeric(substr(meta$site_no, 6, 13))
meta <- meta %>% select(-c(huc_cd))
SP_df <- merge(SP_df, meta, by.x = 'site', by.y = 'site_no')
SP_df <- SP_df %>% select(-c(X))
write.csv(SP_df, './San_Pedro_sigs_attributes.csv', row.names = FALSE)






































# PEARSON ---------------------------------
## BARREN LAND -------------------
nlcd <- read.table('../../../../../My Drive/Overland Flow MS/Data/Catchment Attribute Data/Land Cover/NLCD08_ACC_CONUS.TXT',
                   sep = ',', header = TRUE)
nlcd <- nlcd %>%
  dplyr::select(c(COMID, ACC_NLCD08_31)) %>%
  filter(COMID %in% sites$COMID) 

nlcd <- merge(sites, nlcd, by = 'COMID')
nlcd$GAGE_ID <- as.numeric(substr(nlcd$GAGE_ID, 6, 13))
colnames(nlcd) <- c('COMID', 'site', 'Barren Land')

sigs <- read.csv('./Signature Data/area_weighted_NLDAS/signature_df.csv')
sigs <- sigs %>%
  filter(OF_error_str == 'Warning: Ignoring NaNs in streamflow data. ')

barren_land <- merge(sigs, nlcd, by = 'site')

# Monsoon IE Effect ----
sd_mons_IE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_IE_effect, method = 'pearson', exact = FALSE)
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
sd_mons_SE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_SE_effect, method = 'pearson', exact = FALSE)
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
sd_mons_IE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_IE_thresh, method = 'pearson', exact = FALSE)
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
sd_mons_SE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_SE_thresh, method = 'pearson', exact = FALSE)
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
sd_mons_Storage_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_Storage_thresh, method = 'pearson', exact = FALSE)
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
sd_mons_R_Pvol_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_R_Pvol_RC, method = 'pearson', exact = FALSE)
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
sd_mons_R_Pint_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$mons_R_Pint_RC, method = 'pearson', exact = FALSE)
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
sd_IE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$IE_effect, method = 'pearson', exact = FALSE)
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
sd_SE_effect_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$SE_effect, method = 'pearson', exact = FALSE)
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
sd_IE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$IE_thresh, method = 'pearson', exact = FALSE)
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
sd_SE_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$SE_thresh, method = 'pearson', exact = FALSE)
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
sd_Storage_thresh_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$Storage_thresh, method = 'pearson', exact = FALSE)
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
sd_R_Pvol_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$R_Pvol_RC, method = 'pearson', exact = FALSE)
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
sd_R_Pint_RC_cor <- cor.test(y =barren_land$`Barren Land`, x =barren_land$R_Pint_RC, method = 'pearson', exact = FALSE)
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
sd_mons_IE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$mons_IE_effect, method = 'pearson', exact = FALSE)
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
sd_mons_SE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$mons_SE_effect, method = 'pearson', exact = FALSE)
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
sd_mons_IE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$mons_IE_thresh, method = 'pearson', exact = FALSE)
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
sd_mons_SE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$mons_SE_thresh, method = 'pearson', exact = FALSE)
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
sd_mons_Storage_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$mons_Storage_thresh, method = 'pearson', exact = FALSE)
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
sd_mons_R_Pvol_RC_cor <- cor.test(y =density$`Stream Density`, x =density$mons_R_Pvol_RC, method = 'pearson', exact = FALSE)
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
sd_mons_R_Pint_RC_cor <- cor.test(y =density$`Stream Density`, x =density$mons_R_Pint_RC, method = 'pearson', exact = FALSE)
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
sd_IE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$IE_effect, method = 'pearson', exact = FALSE)
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
sd_SE_effect_cor <- cor.test(y =density$`Stream Density`, x =density$SE_effect, method = 'pearson', exact = FALSE)
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
sd_IE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$IE_thresh, method = 'pearson', exact = FALSE)
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
sd_SE_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$SE_thresh, method = 'pearson', exact = FALSE)
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
sd_Storage_thresh_cor <- cor.test(y =density$`Stream Density`, x =density$Storage_thresh, method = 'pearson', exact = FALSE)
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
sd_R_Pvol_RC_cor <- cor.test(y =density$`Stream Density`, x =density$R_Pvol_RC, method = 'pearson', exact = FALSE)
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
sd_R_Pint_RC_cor <- cor.test(y =density$`Stream Density`, x =density$R_Pint_RC, method = 'pearson', exact = FALSE)
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


## KSAT ------------------------------
ksat <- read.csv('/Users/laurenbolotin/Desktop/san_pedro_ksat.csv')
ksat <- ksat[,1:7]
ksat[,3:7] <- ksat[,3:7]/100

ksat <- merge(sigs, ksat, by.x = 'site', by.y = 'GAGE_ID', all.x = TRUE, all.y = FALSE)


# Monsoon IE Effect ----
ksat_mons_IE_effect_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_IE_effect, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_IE_effect <- ggplot(ksat, aes(y = X_mean, x = mons_IE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_IE_effect_cor)

# Monsoon SE effect ----
ksat_mons_SE_effect_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_SE_effect, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_SE_effect <- ggplot(ksat, aes(y = X_mean, x = mons_SE_effect))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_SE_effect_cor)

# Monsoon IE thresh ----
ksat_mons_IE_thresh_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_IE_thresh, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_IE_thresh <- ggplot(ksat, aes(y = X_mean, x = mons_IE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_IE_thresh_cor)

# Monsoon SE thresh ----
ksat_mons_SE_thresh_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_SE_thresh, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_SE_thresh <- ggplot(ksat, aes(y = X_mean, x = mons_SE_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_SE_thresh_cor)

# Monsoon Storage thresh ----
ksat_mons_Storage_thresh_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_Storage_thresh, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_Storage_thresh <- ggplot(ksat, aes(y = X_mean, x = mons_Storage_thresh))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_Storage_thresh_cor)

# Monsoon R Pvol ----
ksat_mons_R_Pvol_RC_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_R_Pvol_RC, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_R_Pvol_RC <- ggplot(ksat, aes(y = X_mean, x = mons_R_Pvol_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_R_Pvol_RC_cor)

# Monsoon R Pint ----
ksat_mons_R_Pint_RC_cor <- cor.test(y =ksat$X_mean, x =ksat$mons_R_Pint_RC, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_mons_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_mons_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_mons_R_Pint_RC <- ggplot(ksat, aes(y = X_mean, x = mons_R_Pint_RC))+
  geom_point(color = '#46ACC8')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#46ACC8')+
  labs(y = '', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_mons_R_Pint_RC_cor)

#  IE Effect ----
ksat_IE_effect_cor <- cor.test(y =ksat$X_mean, x =ksat$IE_effect, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_IE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_IE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_IE_effect <- ggplot(ksat, aes(y = X_mean, x = IE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#DD8D29')+
  labs(y = 'Stream ksat', x = 'IE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_IE_effect_cor)

#  SE effect ----
ksat_SE_effect_cor <- cor.test(y =ksat$X_mean, x =ksat$SE_effect, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_SE_effect_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_SE_effect_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_SE_effect <- ggplot(ksat, aes(y = X_mean, x = SE_effect))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream ksat', x = 'SE Effect')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_SE_effect_cor)

#  IE thresh ----
ksat_IE_thresh_cor <- cor.test(y =ksat$X_mean, x =ksat$IE_thresh, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_IE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_IE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_IE_thresh <- ggplot(ksat, aes(y = X_mean, x = IE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream ksat', x = 'IE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_IE_thresh_cor)

#  SE thresh ----
ksat_SE_thresh_cor <- cor.test(y =ksat$X_mean, x =ksat$SE_thresh, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_SE_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_SE_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_SE_thresh <- ggplot(ksat, aes(y = X_mean, x = SE_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream ksat', x = 'SE Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_SE_thresh_cor)

#  Storage thresh ----
ksat_Storage_thresh_cor <- cor.test(y =ksat$X_mean, x =ksat$Storage_thresh, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_Storage_thresh_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_Storage_thresh_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_Storage_thresh <- ggplot(ksat, aes(y = X_mean, x = Storage_thresh))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream ksat', x = 'Storage Thresh')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_Storage_thresh_cor)

#  R Pvol ----
ksat_R_Pvol_RC_cor <- cor.test(y =ksat$X_mean, x =ksat$R_Pvol_RC, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_R_Pvol_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_R_Pvol_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_R_Pvol_RC <- ggplot(ksat, aes(y = X_mean, x = R_Pvol_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream ksat', x = 'R Pvol RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_R_Pvol_RC_cor)

#  R Pint ----
ksat_R_Pint_RC_cor <- cor.test(y =ksat$X_mean, x =ksat$R_Pint_RC, method = 'pearson', exact = FALSE)
text_rho <- grobTree(textGrob(paste0('rho = ', round(ksat_R_Pint_RC_cor[["estimate"]],3)), x = 0.60,  y = 0.1, hjust=0,
                              gp=gpar(col="black", fontsize=10)))
text_p <- grobTree(textGrob(paste0('p-value = ', round(ksat_R_Pint_RC_cor[["p.value"]],3)), x = 0.60,  y = 0.2, hjust=0,
                            gp=gpar(col="black", fontsize=10)))
ksat_R_Pint_RC <- ggplot(ksat, aes(y = X_mean, x = R_Pint_RC))+
  geom_point(color = '#DD8D29')+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color = '#DD8D29')+
  labs(y = 'Stream ksat', x = 'R Pint RC')+
  theme_bw()+ 
  annotation_custom(text_rho)+
  annotation_custom(text_p)
rm(ksat_R_Pint_RC_cor)

ksat_IE_grid <- plot_grid(ksat_IE_effect, ksat_mons_IE_effect,
                          ksat_IE_thresh, ksat_mons_IE_thresh,
                          ksat_R_Pint_RC, ksat_mons_R_Pint_RC,
                          ncol = 2)
ksat_IE_grid

ksat_SE_grid <- plot_grid(ksat_SE_effect, ksat_mons_SE_effect,
                          ksat_SE_thresh, ksat_mons_SE_thresh,
                          ksat_R_Pvol_RC, ksat_mons_R_Pvol_RC,
                          ksat_Storage_thresh, ksat_mons_Storage_thresh,
                          ncol = 2)
ksat_SE_grid

