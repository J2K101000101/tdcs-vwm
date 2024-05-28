% % Compare the model and get the parameters:
% % Require: 
% % 0. MemToolbox (Suchow et al., 2013)
% % 1. datastruct.m 
% % 2. compareModels.m
% % 3. getParamters_tDCS.m 

% input: ~/cleaned/ort_tdcs.csv

% output: 
% 1. overal ~/ICs.mat in cleaned folder; based on overal ICs: cleaned/outputICS/dAIC_M1_M2.csv
% & cleaned/outputICS/dBIC_M1_M2.csv & cleaned/outputICS/eachCondFit.csv folder for future
% plotting needs
% 2. ~/output/capacity_*.csv & precision_*.csv 

% MATLAB R2021a
%% 
clear all
cd (uigetdir()); % * you need to go to default work directory where contains this script!
default = pwd;
outputFolder = [default, '/','output'];
cd cleaned

%all
File = dir('ort_tdcs.csv');

%% run model comparison first 

compareModels(File);%Standard Mixture Model is the better fit

%model = SwapModel(); 
model = StandardMixtureModel();
getParameters_tDCS (File,outputFolder,model);

% Next:go to tdcs_3_analysis.R
