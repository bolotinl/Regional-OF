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

sp.intsct.to_csv('../San Pedro River/NLDAS_intsct.csv')

# # Somehow within the climrods code, extract the following info:
# watershed_file = watershed
# watershed_file = pd.DataFrame(watershed.drop(columns='geometry'))
# watershed_file['AREA_km2']=watershed_area
# watershed_file.to_csv('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River/watershed_areas.csv')