# Analyze hydrologic signature results for the San Pedro River Watershed
library(dataRetrieval)
library(tidyverse)
library(glue)
library(lubridate)
library(data.table)
library(cowplot)


setwd('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River/Signature Data')
monsoon_files <- list.files(path = getwd(), pattern = 'monsoon.csv')
full_files <- list.files(path = getwd(), pattern = 'all.csv')

monsoon_sigs <- lapply(monsoon_files, read.csv)
monsoon_sigs <- rbindlist(monsoon_sigs)

full_sigs <- lapply(full_files, read.csv)
full_sigs <- rbindlist(full_sigs)

colnames(monsoon_sigs) <- paste("mons_", colnames(monsoon_sigs), sep = '')
monsoon_sigs <- monsoon_sigs %>%
  rename(site = mons_site)

sigs_df <- merge(full_sigs, monsoon_sigs, by = 'site')
rm(monsoon_sigs, full_sigs)

names(sigs_df)
diff_df <- sigs_df %>%
  mutate(IE_effect_diff = IE_effect - mons_IE_effect,
         SE_effect_diff = SE_effect - mons_SE_effect,
         IE_thresh_signif_diff = IE_thresh_signif - mons_IE_thresh_signif,
         SE_thresh_signif_diff = SE_thresh_signif - mons_SE_thresh_signif,
         IE_thresh_diff = IE_thresh - mons_IE_thresh,
         SE_thresh_diff = SE_thresh - mons_SE_thresh,
         SE_slope_diff = SE_slope - mons_SE_slope,
         Storage_thresh_signif_diff = Storage_thresh_signif - mons_Storage_thresh_signif,
         Storage_thresh_diff = Storage_thresh - mons_Storage_thresh,
         min_Qf_perc_diff = min_Qf_perc - mons_min_Qf_perc)
diff_df <- diff_df %>%
  filter(IE_effect != 'NaN')

diff_df$site <- as.factor(diff_df$site)

## DUMBBELL PLOTS----------------------------------
p_db_IE_effect <- ggplot()+
  geom_segment(data = diff_df, aes(x = IE_effect, y = site, xend = mons_IE_effect, yend = site),
               color = ifelse(diff_df$IE_effect_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_IE_effect, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = IE_effect, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'IE Effect')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

p_db_SE_effect <- ggplot()+
  geom_segment(data = diff_df, aes(x = SE_effect, y = site, xend = mons_SE_effect, yend = site),
               color = ifelse(diff_df$SE_effect_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_SE_effect, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = SE_effect, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'SE Effect')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

p_db_IE_thresh_signif <- ggplot()+
  geom_segment(data = diff_df, aes(x = IE_thresh_signif, y = site, xend = mons_IE_thresh_signif, yend = site),
               color = ifelse(diff_df$IE_thresh_signif_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_IE_thresh_signif, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = IE_thresh_signif, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'IE Threshold Significance')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

p_db_SE_thresh_signif <- ggplot()+
  geom_segment(data = diff_df, aes(x = SE_thresh_signif, y = site, xend = mons_SE_thresh_signif, yend = site),
               color = ifelse(diff_df$SE_thresh_signif_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_SE_thresh_signif, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = SE_thresh_signif, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'SE Threshold Significance')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

p_db_IE_thresh <- ggplot()+
  geom_segment(data = diff_df, aes(x = IE_thresh, y = site, xend = mons_IE_thresh, yend = site),
               color = ifelse(diff_df$IE_thresh_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_IE_thresh, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = IE_thresh, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'IE Threshold')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

p_db_SE_thresh <- ggplot()+
  geom_segment(data = diff_df, aes(x = SE_thresh, y = site, xend = mons_SE_thresh, yend = site),
               color = ifelse(diff_df$SE_thresh_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_SE_thresh, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = SE_thresh, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'SE Threshold')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


p_db_SE_slope <- ggplot()+
  geom_segment(data = diff_df, aes(x = SE_slope, y = site, xend = mons_SE_slope, yend = site),
               color = ifelse(diff_df$SE_slope_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_SE_slope, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = SE_slope, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'SE Slope')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


p_db_Storage_thresh_signif <- ggplot()+
  geom_segment(data = diff_df, aes(x = Storage_thresh_signif, y = site, xend = mons_Storage_thresh_signif, yend = site),
               color = ifelse(diff_df$Storage_thresh_signif_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_Storage_thresh_signif, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = Storage_thresh_signif, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'Storage Threshold Significance')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


p_db_Storage_thresh <- ggplot()+
  geom_segment(data = diff_df, aes(x = Storage_thresh, y = site, xend = mons_Storage_thresh, yend = site),
               color = ifelse(diff_df$Storage_thresh_diff > 0, 'indianred', 'seagreen'),
               size = 1.5, #Note that I sized the segment to fit the points
               alpha = .6)+
  geom_point(data = diff_df, aes(x = mons_Storage_thresh, y = site), size = 2, color = 'deepskyblue3')+
  geom_point(data = diff_df, aes(x = Storage_thresh, y = site), size = 2, color = 'darkgoldenrod1')+
  labs(x = '', y = 'Gage ID', title = 'Storage Threshold')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

dumbbell_grid <- plot_grid(p_db_IE_effect, p_db_SE_effect, p_db_IE_thresh, p_db_SE_thresh, p_db_IE_thresh_signif, p_db_SE_thresh_signif, p_db_SE_slope, p_db_Storage_thresh, p_db_Storage_thresh_signif, ncol = 3)
print(dumbbell_grid)
