clearvars
close all

warning('off','MATLAB:table:ModifiedAndSavedVarnames')
warning('off','MATLAB:table:RowsAddedExistingVars')

%Phytoplankton and microbial time-series
NanoMicroBiomass = readtable('..\Collaborative Data\Nano & Microplankton Biomass (CalCOFI).csv');
PicoBiomass = readtable('..\Collaborative Data\Picoplankton Biomass (CalCOFI).csv');
SizeFrac = readtable('..\Collaborative Data\Size-Fractionated Chl (CalCOFI).csv');

%Zooscan time-series
Appendicularia = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Appendicularia.csv');
Calanoida = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Copepoda_Calanoida.csv');
Eucalanids = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Copepoda_Eucalanids.csv');
Doliolids = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Doliolids.csv');
Pyrosomes = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Pyrosomes.csv');
Salps = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Salps.csv');
Euphausiids = readtable('..\Collaborative Data\ZooscanDB\PROPOOS_data_export_Euphausiids.csv');

%Append latitude and longitude to Zooscan time-series
CalCOFIStationData = readtable('CalCOFIStationData.xlsx');
[Appendicularia.lat,Appendicularia.lon] = Station2llCC(Appendicularia.Line,Appendicularia.Station,CalCOFIStationData);
[Calanoida.lat,Calanoida.lon] = Station2llCC(Calanoida.Line,Calanoida.Station,CalCOFIStationData);
[Eucalanids.lat,Eucalanids.lon] = Station2llCC(Eucalanids.Line,Eucalanids.Station,CalCOFIStationData);
[Doliolids.lat,Doliolids.lon] = Station2llCC(Doliolids.Line,Doliolids.Station,CalCOFIStationData);
[Pyrosomes.lat,Pyrosomes.lon] = Station2llCC(Pyrosomes.Line,Pyrosomes.Station,CalCOFIStationData);
[Salps.lat,Salps.lon] = Station2llCC(Salps.Line,Salps.Station,CalCOFIStationData);
[Euphausiids.lat,Euphausiids.lon] = Station2llCC(Euphausiids.Line,Euphausiids.Station,CalCOFIStationData);

%CalCOFI BottleData Data
BottleData = readtable('..\Collaborative Data\CalCOFI Bottle Data (1970 - 2021).csv');
tmp = char(BottleData.Sta_ID);
BottleData.Line = str2num(tmp(:,1:5));
BottleData.Station = str2num(tmp(:,7:end));
[BottleData.lat,BottleData.lon] = Station2llCC(BottleData.Line,BottleData.Station,CalCOFIStationData);
BottleData = removevars(BottleData,{'Depth_ID','Salnty_PSU_','STheta','O2ml_L','BtlNum','RecInd','Phaqua','SiO3qu',....
    'NO2q'});
CruiseAverages = readtable('..\Collaborative Data\CalCOFI Cruise Averages.csv');
CruiseAverages.Date = datetime(floor(CruiseAverages.Year),1,floor(rem(CruiseAverages.Year,1)*365.25));
StationData = readtable('..\Collaborative Data\CalCOFI Station Data.csv');

%Disturbance Timeseries
BEUTI_daily = readtable('..\Collaborative Data\BEUTI_daily.csv');
BEUTI_daily.Date = datetime(BEUTI_daily.year,BEUTI_daily.month,BEUTI_daily.day);
CUTI_daily = readtable('..\Collaborative Data\CUTI_daily.csv');
CUTI_daily.Date = datetime(CUTI_daily.year,CUTI_daily.month,CUTI_daily.day);
SanDiegoSLA = readtable('..\Collaborative Data\SanDiego SeaLevel and Anomalies.csv');

clear tmp CalCOFIStationData

save('..\Processed Data\TimeSeries.mat')