clearvars
close all

load('..\Processed Data\TimeSeries.mat')

clearvars -except StationData NanoMicroBiomass PicoBiomass

%CalCOFI Data
NPP = StationData;
NPP(find(isnan(NPP.IntC14_mg_m__)),:)=[];

%Picoplankton biomass
stations = unique(PicoBiomass.DatetimeUTC);
stations(find(isnat(stations)))=[];
for i=1:length(stations)
    inds = find(PicoBiomass.DatetimeUTC==stations(i));
    tmp = PicoBiomass(inds,:);
    PicoBiomass_int(i,1:8) = tmp(1,1:8);
    PicoBiomass_int.HeterotrophicBacteria_ML_ug_L(i) = tmp.HeterotrophicBacteria__g_L_(1);
    PicoBiomass_int.Prochlorococcus_ML_ug_L(i) = tmp.Prochlorococcus__g_L_(1);
    PicoBiomass_int.Synechococcus_ML_ug_L(i) = tmp.Synechococcus__g_L_(1);
    [int] = vertint(tmp.HeterotrophicBacteria__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    PicoBiomass_int.HeterotrophicBacteria_VI_mg_m3(i) = int;
    [int] = vertint(tmp.Prochlorococcus__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    PicoBiomass_int.Prochlorococcus_VI_mg_m3(i) = int;
    [int] = vertint(tmp.Synechococcus__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    PicoBiomass_int.Synechococcus_VI_mg_m3(i) = int;
end


%NanoMicroplankton biomass
stations = unique(NanoMicroBiomass.DatetimeGMT);
stations(find(isnat(stations)))=[];
for i=1:length(stations)
    inds = find(NanoMicroBiomass.DatetimeGMT==stations(i));
    tmp = NanoMicroBiomass(inds,:);
    NanoMicroBiomass_int(i,1:9) = tmp(1,[1:3,6:11]);
    NanoMicroBiomass_int.TotalDiatom_ML_ug_L(i) = tmp.TotalDiatom__g_L_(1);
    NanoMicroBiomass_int.TotalAutoDinoflag_ML_ug_L(i) = tmp.TotalAutoDinoflag__g_L_(1);
    NanoMicroBiomass_int.TotalAutoEuk_ML_ug_L(i) = tmp.TotalAutoEuk__g_L_(1);
    NanoMicroBiomass_int.TotalHeteroEuk_ML_ug_L(i) = tmp.TotalHeteroEuk__g_L_(1);
    [int] = vertint(tmp.TotalDiatom__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    NanoMicroBiomass_int.TotalDiatom_VI_mg_m3(i) = int;
    [int] = vertint(tmp.TotalAutoDinoflag__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    NanoMicroBiomass_int.TotalAutoDinoflag_VI_mg_m3(i) = int;
    [int] = vertint(tmp.TotalAutoEuk__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    NanoMicroBiomass_int.TotalAutoEuk_VI_mg_m3(i) = int;
    [int] = vertint(tmp.TotalHeteroEuk__g_L_,tmp.Depth_m_,tmp.Depth_m_(end));
    NanoMicroBiomass_int.TotalHeteroEuk_VI_mg_m3(i) = int;
end

save('..\Processed Data\VertInt & Surface Timeseries.mat','PicoBiomass_int','NanoMicroBiomass_int','NPP')