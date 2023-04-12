%%
close all
% clear all
clc

mydir = pwd;

%%
cd '/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/San Pedro River/'
addpath(genpath('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/Data and Scripts/Regional-OF'))
addpath(genpath('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Regional Overland Flow Project/BrewerMap'))

%%
data = readtable('San_Pedro_sigs_attributes.csv', 'Delimiter',',');

%%
plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.IE_effect,'attribute_name','IE Effect',...
'c_limits',[0 1],'figure_title','IE Effect');

plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.IE_thresh,'attribute_name','IE Threshold (mm)',...
'c_limits',[0 1],'figure_title','IE Threshold');
% Add timescale to data table

plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.IE_thresh_signif,'attribute_name','IE_thresh_signif',...
'c_limits',[0 1],'figure_title','IE Threshold Significance');

plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.R_Pint_RC,'attribute_name','R_Pint_RC',...
'c_limits',[0 1],'figure_title','Runoff vs. Precipitation Intensity');


plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.R_Pvol_RC,'attribute_name','R_Pvol_RC',...
'c_limits',[0 1],'figure_title','Runoff vs. Precipitation Volume');


plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.SE_slope,'attribute_name','SE Slope (mm/hr)',...
'c_limits',[0 1],'figure_title','SE Slope');

plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.BarrenLand,'attribute_name','%',...
'c_limits',[0 1],'figure_title','Barren Land');

plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.StreamDensity,'attribute_name','',...
'c_limits',[0 1],'figure_title','Stream Density');

plotMapUS(data.dec_lat_va,data.dec_long_va,...
data.ksat_mean,'attribute_name','mm/hr',...
'c_limits',[0 1],'figure_title','Mean Ksat');

%   lat: latitude
%   lon: longitude
%   z: attribute to be coloured in, e.g. BFI
%   attribute_name: name of attribute
%   ID: catchment ID
%   colour_scheme: name of colour scheme
%   flip_colour_scheme: flip colour scheme?
%   c_limits: limits of colour axis, e.g. [0 1]
%   c_lower_limit_open: is the lower limit open?
%   c_upper_limit_open: is the upper limit open?
%   nr_colours: nr of colours used for colourscale
%   figure_title: title of plot, e.g. '(a)'
%   figure_name: name for saving, e.g. CAMELS_BFI
%   save_figure: save plot true/false
%   figure_path: path to folder where figure should be saved
%   figure_type: figure type, e.g. -dpdf or -dmeta
