function [lat,lon] = Station2llCC(line,station,CalCOFIStationData)

% clearvars
% close all
% CalCOFIStationData = readtable('CalCOFIStationData.xlsx');
% line = 93.2
% station = 41

lat = griddata(CalCOFIStationData.LINE,CalCOFIStationData.STA,CalCOFIStationData.LAT_DEC_,line,station);
lon = griddata(CalCOFIStationData.LINE,CalCOFIStationData.STA,CalCOFIStationData.LON_DEC_,line,station);
