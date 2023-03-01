## Regional-OF
The code workflow goes as follows:
1) San_Pedro.R: Identify and download streamflow data for USGS sites.
2) NLDAS_data.py: Download and calculate area weighted precipitation data for sites.
3) San_Pedro.R: Once you have the precipitation data, use the rest of this script to pair it with the streamflow data.
4) San_Pedro_Monsoon_Sigs.m: Calculate hydrologic signatures.
5) San_Pedro_Sig_Analysis.R: Analyze differences between year-round signature values and monsoon season signature values.
6) San_Pedro_ComID_NHD.R: Pair catchment attributes with sites and plot correlations between signatures and attributes. (This one is certainly not optimized -- sorry ahead of time!)