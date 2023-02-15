import sys
import os
os.getcwd()

sys.path.insert(0, '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/climrods')

from climrods import *

start_dt = '1980-01-01'
end_dt = '2023-02-01'
start_hr = '00'
end_hr = '23'

sp = NLDAS_Downloader(start_dt, end_dt, start_hr, end_hr)

grid_path = '/Volumes/GoogleDrive/My Drive/Python_Final/climrods/sample_data/NLDAS_Grid_Reference.shp'
shp_out_path = '../San Pedro River/san_pedro_watershed_shapefile.shp'

sp.watershed_from_gauge('../San Pedro River/Q_site_info.csv', 'site_no', shp_out_path)

sp.intersect_watershed(shp_out_path, grid_path)

sp.url_builder()

sp.out_dir = '../San Pedro River/NLDAS_climrods/'
# sp.download('../San Pedro River/NLDAS_climrods/')

weight_dir = '../San Pedro River/NLDAS_climrods/weighted_P/'
sp.area_weight_P(weight_dir)