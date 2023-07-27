function [output] = StandardTimeseriesPlots(station)

%station = [80,55];
output = 0;

load('..\Processed Data\OutputPlottedData1.mat')
load('..\Processed Data\TimeSeries.mat')

%Phytoplankton
fighandle = figure(12);
fighandle.Units = 'inches';
fighandle.Position = [3 1 7 7];
subplot(2,2,1)
ind = find(NanoMicroBiomass_int.Line==station(1) & NanoMicroBiomass_int.Station==station(2));
plot(NanoMicroBiomass_int.DatetimeGMT(ind),PhyBiomass(ind),'ok','MarkerFaceColor',[0 0.7 0])
ylabel('Total Phytoplankton Biomass (mg C m^-^3)')
yyaxis right
plot(SanDiegoSLA.Date,SanDiegoSLA.SeaLevel_LongTermTrendAndSeasonalCycleRemoved_mm_,'r')  %ENSO proxy
xlim([min(NanoMicroBiomass_int.DatetimeGMT(ind)),max(NanoMicroBiomass_int.DatetimeGMT(ind))])
set(gca,'FontSize',9)
title(['Station ',num2str(station(1)),'.',num2str(station(2))])
ylim([-200 200])


%Heterotrophic Protists
subplot(2,2,3)
ind = find(NanoMicroBiomass_int.Line==station(1) & NanoMicroBiomass_int.Station==station(2));
plot(NanoMicroBiomass_int.DatetimeGMT(ind),NanoMicroBiomass_int.TotalHeteroEuk_VI_mg_m3(ind),'ok','MarkerFaceColor',[1 0 1])
ylabel('Heterotrophic Protist Biomass (mg C m^-^3)')
yyaxis right
plot(SanDiegoSLA.Date,SanDiegoSLA.SeaLevel_LongTermTrendAndSeasonalCycleRemoved_mm_,'r')  %ENSO proxy
xlim([min(NanoMicroBiomass_int.DatetimeGMT(ind)),max(NanoMicroBiomass_int.DatetimeGMT(ind))])
set(gca,'FontSize',9)
title(['Station ',num2str(station(1)),'.',num2str(station(2))])
ylim([-200 200])


%NPP
subplot(2,2,2)
ind = find(NPP.Rpt_Line==station(1) & NPP.Rpt_Sta==station(2));
plot(NPP.Date(ind),NPP.IntChl_mg_m__(ind),'ok','MarkerFaceColor',[0 1 1])
ylabel(['Net Primary Production',char(10),'(mg C m^-^2 / half photoperiod)'])
yyaxis right
plot(SanDiegoSLA.Date,SanDiegoSLA.SeaLevel_LongTermTrendAndSeasonalCycleRemoved_mm_,'r')  %ENSO proxy
xlim([min(NPP.Date(ind)),max(NPP.Date(ind))])
set(gca,'FontSize',9)
title(['Station ',num2str(station(1)),'.',num2str(station(2))])
ylim([-200 200])


%Metazoan Herbivores
subplot(2,2,4)
ind = find(Herbivores.Line==station(1) & Herbivores.Station==station(2) & strcmp(Herbivores.DayOrNight,'Night'));
plot(Herbivores.StationDate(ind),TotHerbivores(ind),'ok','MarkerFaceColor',[0.6 0.2 0.7])
ylabel(['Herbivorous Metazoan Biomass',char(10),'(mg C m^-^2)'])
yyaxis right
plot(SanDiegoSLA.Date,SanDiegoSLA.SeaLevel_LongTermTrendAndSeasonalCycleRemoved_mm_,'r')  %ENSO proxy
xlim([min(Herbivores.StationDate(ind)),max(Herbivores.StationDate(ind))])
set(gca,'FontSize',9)
title(['Station ',num2str(station(1)),'.',num2str(station(2))])
ylim([-200 200])

exportgraphics(gcf,['..\Figures\','StandardTimeseriesPlots.Station',num2str(station(1)),'.',num2str(station(2)),'.png'],'Resolution',600)