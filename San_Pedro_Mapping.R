library(tidyverse)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggspatial)
library(viridis)
library(viridisLite)
library(cowplot)
# devtools::install_github('UrbanInstitute/urbnmapr')
library(urbnmapr)
library(glue)
library(nhdplusTools)

# usgs_plot <-  plot_nhdplus(list('nwissite', 'USGS-09473100'))
# 
# nwissites <- list(featureSource = 'nwissite', featureID = 'USGS-09473100')
# flowlines <- navigate_nldi(nwissites, mode = 'upstreamTributaries', data_source = 'flowlines')
# nhdplus <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
#                           output_file = file.path(getwd(), "nhdplus.gpkg"),
#                           nhdplus_data = "download",
#                           overwrite = TRUE, return_data = FALSE)
# flowline <- read_sf(nhdplus, "NHDFlowline_Network")



setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River')
sigs_att <- read.csv('San_Pedro_sigs_attributes.csv')

# states <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
# az <- urbnmapr::states %>% filter(state_name == 'Arizona')
# az_counties <- urbnmapr::countydata %>% 
#   left_join(counties, by = 'county_fips') %>%
#   filter(state_name == 'Arizona')
# 
# theme_set(theme_minimal())
# 
# p_IE_effect <- ggplot() +
#   geom_polygon(data = az_counties, aes(long,lat, group = group), fill = 'grey', color = 'grey80')+
#   geom_point(data = sigs_att, mapping = aes(x = dec_long_va, y = dec_lat_va, fill = IE_effect), size = 1, shape = 21, color = "black")+
#   # geom_point(data = coords, mapping = aes(x = longs, y = lats), fill = "magenta2", size = 1, shape = 21, color = "black")+ # These are the sites from Hampton& Basu 2022; get "coords" from 1_3_Get_nonCAMELS_Watersheds.R
#   labs(x = "", y = "", title = "")+
#   # annotation_scale(location = "bl", width_hint = 0.5) +
#   # annotation_north_arrow(location = "bl", which_north = "true", 
#   #                        pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
#   #                        style = north_arrow_fancy_orienteering)+
#   scale_fill_viridis(option = "D")+
#   theme(plot.margin = margin(0,0,0,0))
# 

# Save plots of points over SP watershed
sp <- st_read('./san_pedro_watershed_shapefile.shp')
sp$GAGE_ID <- as.numeric(sp$GAGE_ID)
sp <- merge(sp, sigs_att, by.x = 'GAGE_ID', by.y = 'site')

plot_items <- c(names(sp)[15:23], names(sp)[25:26], names(sp)[28:36], names(sp)[38:39], names(sp)[41:52])

grid_fun <- function(x){
  var <- plot_items[x]
  
  sp <- sp %>%
    filter(GAGE_ID != 9470820)
  
  p <<- ggplot(sp)+
    geom_sf()+
    labs(x = '', y = '', title = var, fill = '')+
    scale_fill_viridis(option = "D")+
    geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = get(paste0(plot_items[x]))), size = 2, shape = 21, color = "black")+
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    theme(axis.text = element_blank())+
    theme(plot.margin = margin(0,0,0,0))
  
  
  assign(paste0('p_', plot_items[x]), p)
  
  p_name <- glue('../Figures/Maps/ws_map_{var}.png')
  ggsave(p_name, plot = p, width = 5, height = 5, units = c('in'), dpi = 300, bg = 'white')
  rm(p)
}

lapply(seq(1, length(plot_items)), grid_fun)


sp <- sp %>%
  filter(GAGE_ID != 9470820)

p_IE_effect <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'IE_effect', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = IE_effect), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_IE_thresh <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'IE_thresh', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = IE_thresh), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_IE_thresh_signif <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'IE_thresh_signif', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = IE_thresh_signif), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_R_Pint_RC <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'R_Pint_RC', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = R_Pint_RC), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_IE_effect <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_IE_effect', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_IE_effect), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))


p_mons_IE_thresh <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_IE_thresh', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_IE_thresh), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))


p_mons_IE_thresh_signif <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_IE_thresh_signif', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_IE_thresh_signif), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_R_Pint_RC <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_R_Pint_RC', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_R_Pint_RC), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))



p_IE_grid <- plot_grid(p_IE_effect, p_IE_thresh, p_IE_thresh_signif, p_R_Pint_RC,
                       p_mons_IE_effect, p_mons_IE_thresh, p_mons_IE_thresh_signif, p_mons_R_Pint_RC,
                       nrow = 2)
p_IE_grid


p_SE_effect <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'SE_effect', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = SE_effect), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_SE_thresh <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'SE_thresh', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = SE_thresh), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_SE_thresh_signif <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'SE_thresh_signif', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = SE_thresh_signif), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_R_Pvol_RC <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'R_Pvol_RC', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = R_Pvol_RC), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_SE_slope <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'SE_slope', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = SE_slope), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_Storage_thresh_signif <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'Storage_thresh_signif', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = Storage_thresh_signif), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_Storage_thresh <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'Storage_thresh', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = Storage_thresh), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_SE_effect <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_SE_effect', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_SE_effect), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))


p_mons_SE_thresh <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_SE_thresh', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_SE_thresh), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))


p_mons_SE_thresh_signif <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_SE_thresh_signif', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_SE_thresh_signif), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_R_Pvol_RC <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_R_Pvol_RC', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_R_Pvol_RC), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_SE_slope <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_SE_slope', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_SE_slope), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_Storage_thresh_signif <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_Storage_thresh_signif', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_Storage_thresh_signif), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_mons_Storage_thresh <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'mons_Storage_thresh', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = mons_Storage_thresh), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))



p_SE_grid <- plot_grid(p_SE_effect, p_SE_thresh, p_SE_thresh_signif, p_R_Pvol_RC, p_SE_slope, p_Storage_thresh, p_Storage_thresh_signif,
                       p_mons_SE_effect, p_mons_SE_thresh, p_mons_SE_thresh_signif, p_mons_R_Pvol_RC, p_mons_SE_slope, p_mons_Storage_thresh, p_mons_Storage_thresh_signif,
                       nrow = 2)
p_SE_grid


p_Barren_Land <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'Barren.Land', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = Barren.Land), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_Stream.Density <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'Stream.Density', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = Stream.Density), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_ksat_mean <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'ksat_mean', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = ksat_mean), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_DTBR_mean <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'DTBR_mean', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = DTBR_mean), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_permeability <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'permeability', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = permeability), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_ksat_median <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'ksat_median', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = ksat_median), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

p_DTBR_median <- ggplot(sp)+
  geom_sf()+
  labs(x = '', y = '', title = 'DTBR_median', fill = '')+
  scale_fill_viridis(option = "D")+
  geom_point(mapping = aes(x = dec_long_va, y = dec_lat_va, fill = DTBR_median), size = 2, shape = 21, color = "black")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  theme(axis.text = element_blank())+
  theme(plot.margin = margin(3,0,0,0))

attr_grid <- plot_grid(p_Barren_Land, p_Stream.Density, p_ksat_mean, p_ksat_median, p_DTBR_mean, p_DTBR_median, 
                       p_permeability, ncol = 4)
attr_grid
