%%
close all
% clear all
clc

% Navigate to TOSSH calculation functions
mydir = pwd;

%%
cd ../../../../../'Shared drives'/'Lauren and Hilary'/'Regional Overland Flow Project'/'Data and Scripts'/'San Pedro River'/Monsoon_Data/

%% prepare for OF signatures
addpath(genpath('/Volumes/GoogleDrive/My Drive/Overland Flow MS/TOSSH-master/TOSSH_code'))
addpath(genpath('/Volumes/GoogleDrive/Shared drives/Lauren and Hilary/Data from Hilary/WuCode'))

%% iterate through Q/Forcing Data
% HOURLY DATA --------------------------------------------------------------

% List all files to iterate through
files = dir(fullfile(pwd, '*uv.csv'));

% Create consistent components of output filepaths
folder_name = '../Signature Data/';
file_name_end_total = '_uv_results_monsoon.csv';

% run i = 17 manually and change line 33 column name
% 16 is weird for Wu
for i = 18:length(files)
    data = readtable(files(i).name);    
    site8 = files(i).name(1:8);
    site = str2double(site8);

    % Create inputs for signature function
    Q_mat = {data.Q_mm_hr};
    % Q_mat = {data.QObs_mmh}
    t_mat = {data.date};
    P_mat = {data.total_precipitation};

    % Calculate signatures
    site8
    %results = calc_McMillan_OverlandFlow(Q_mat, t_mat, P_mat, plot_results = true);
    results = calc_McMillan_OverlandFlow_withWu(Q_mat, t_mat, P_mat);
    results.site = site8;
    
    % Create path names for export files
    total_path = folder_name+string(site8)+file_name_end_total;

    % Export CSVs
    writetable(struct2table(results), total_path);

end
%% iterate through Q/Forcing Data
% DAILY DATA --------------------------------------------------------------

% List all files to iterate through
files = dir(fullfile(pwd, '*dv.csv'));

% Create consistent components of output filepaths
folder_name = '../Signature Data/';
file_name_end_total = '_dv_results_monsoon.csv';


for i = 3:length(files)
    data = readtable(files(i).name);    
    site8 = files(i).name(1:8);
    site = str2double(site8);

    % Create inputs for signature function
    Q_mat = {data.Q_mm_d};
    t_mat = {data.date};
    P_mat = {data.total_precipitation};


    % Calculate signatures
    %results = calc_McMillan_OverlandFlow(Q_mat, t_mat, P_mat);
    results = calc_McMillan_OverlandFlow_withWu(Q_mat, t_mat, P_mat);

    results.site = site8;
    
    % Create path names for export files
    total_path = folder_name+string(site8)+file_name_end_total;

    % Export CSVs
    writetable(struct2table(results), total_path);

end

%% 
cd ../QP_Data/
%% NON-MONSOON
% HOURLY DATA --------------------------------------------------------------

% List all files to iterate through
files = dir(fullfile(pwd, '*uv.csv'));

% Create consistent components of output filepaths
folder_name = '../Signature Data/';
file_name_end_total = '_uv_results_all.csv';

% i = 16 isn't working
for i = 1:length(files)
    data = readtable(files(i).name);    
    site8 = files(i).name(1:8);
    site = str2double(site8);

    % Create inputs for signature function
    Q_mat = {data.Q_mm_hr};
    t_mat = {data.date};
    P_mat = {data.total_precipitation};

    % Calculate signatures
    %results = calc_McMillan_OverlandFlow(Q_mat, t_mat, P_mat);
    results = calc_McMillan_OverlandFlow_withWu(Q_mat, t_mat, P_mat);
    results.site = site8;

    % Create path names for export files
    total_path = folder_name+string(site8)+file_name_end_total;

    % Export CSVs
    writetable(struct2table(results), total_path);

end
%% iterate through Q/Forcing Data
% DAILY DATA --------------------------------------------------------------

% List all files to iterate through
files = dir(fullfile(pwd, '*dv.csv'));

% Create consistent components of output filepaths
folder_name = '../Signature Data/';
file_name_end_total = '_dv_results_all.csv';


for i = 1:length(files)
    data = readtable(files(i).name);    
    site8 = files(i).name(1:8);
    site = str2double(site8);

    % Create inputs for signature function
    Q_mat = {data.Q_mm_d};
    t_mat = {data.date};
    P_mat = {data.total_precipitation};


    % Calculate signatures
    results = calc_McMillan_OverlandFlow_withWu(Q_mat, t_mat, P_mat);
    results.site = site8;
    
    % Create path names for export files
    total_path = folder_name+string(site8)+file_name_end_total;

    % Export CSVs
    writetable(struct2table(results), total_path);

end
