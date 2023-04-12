import sys
import os
os.getcwd()
os.chdir('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/Arizona')

sys.path.insert(0, '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/climrods')

from climrods import *

start_dt = '1980-01-01'
end_dt = '2023-02-01'
start_hr = '00'
end_hr = '23'

az = NLDAS_Downloader(start_dt, end_dt, start_hr, end_hr)

grid_path = '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/sample_data/NLDAS_Grid_Reference.shp'
shp_out_path = './az_watershed_shapefile.shp'
az.watershed_from_gauge('./Q_site_info.csv', 'site_no', shp_out_path)
az.intersect_watershed(shp_out_path, grid_path)
az.url_builder()

az.out_dir = '../Arizona/NLDAS_climrods/'
az.download('../Arizona/NLDAS_climrods/')

weight_dir = '../Arizona/NLDAS_climrods/area_weighted_P/'
az.area_weight_P(weight_dir)
az.intsct.to_csv('../Arizona/NLDAS_intsct_1.csv')

# # Somehow within the climrods code, extract the following info:
# watershed_file = watershed
# watershed_file = pd.DataFrame(watershed.drop(columns='geometry'))
# watershed_file['AREA_km2']=watershed_area
# watershed_file.to_csv('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River/watershed_areas.csv')

az = NLDAS_Downloader(start_dt, end_dt, start_hr, end_hr)

grid_path = '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/sample_data/NLDAS_Grid_Reference.shp'
shp_out_path = './az_watershed_shapefile_2.shp'
az.watershed_from_gauge('./Q_site_info_2.csv', 'site_no', shp_out_path)
az.intersect_watershed(shp_out_path, grid_path)
az.url_builder()

az.out_dir = '../Arizona/NLDAS_climrods/'
az.download('../Arizona/NLDAS_climrods/')

weight_dir = '../Arizona/NLDAS_climrods/area_weighted_P/'
az.area_weight_P(weight_dir)
az.intsct.to_csv('../Arizona/NLDAS_intsct_2.csv')


az = NLDAS_Downloader(start_dt, end_dt, start_hr, end_hr)

grid_path = '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/sample_data/NLDAS_Grid_Reference.shp'
shp_out_path = './az_watershed_shapefile_3.shp'
az.watershed_from_gauge('./Q_site_info_3.csv', 'site_no', shp_out_path)
az.intersect_watershed(shp_out_path, grid_path)
az.url_builder()

az.out_dir = '../Arizona/NLDAS_climrods/'
az.download('../Arizona/NLDAS_climrods/')

weight_dir = '../Arizona/NLDAS_climrods/area_weighted_P/'
az.area_weight_P(weight_dir)
az.intsct.to_csv('../Arizona/NLDAS_intsct_3.csv')


az = NLDAS_Downloader(start_dt, end_dt, start_hr, end_hr)

grid_path = '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/sample_data/NLDAS_Grid_Reference.shp'
shp_out_path = './az_watershed_shapefile_4.shp'
az.watershed_from_gauge('./Q_site_info_4.csv', 'site_no', shp_out_path)
az.intersect_watershed(shp_out_path, grid_path)
az.url_builder()

az.out_dir = '../Arizona/NLDAS_climrods/'
az.download('../Arizona/NLDAS_climrods/')

weight_dir = '../Arizona/NLDAS_climrods/area_weighted_P/'
az.area_weight_P(weight_dir)
az.intsct.to_csv('../Arizona/NLDAS_intsct_4.csv')
