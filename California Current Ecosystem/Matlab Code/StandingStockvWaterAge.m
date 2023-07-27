clearvars
close all

load('..\Processed Data\TimeSeries w waterparcel ages.mat')

%-----------Phytoplankton----------------------------------------------
PicoBiomass_int.Prochlorococcus_VI_mg_m3(find(isnan(PicoBiomass_int.Prochlorococcus_VI_mg_m3)))=0;
for i=1:height(NanoMicroBiomass_int)
    ind = find(strcmp(NanoMicroBiomass_int.studyName(i),PicoBiomass_int.studyName) & NanoMicroBiomass_int.EventNumber(i)==PicoBiomass_int.EventNumber);
    if length(ind)==1
        NanoMicroBiomass_int.Prochlorococcus_VI_mg_m3(i) = PicoBiomass_int.Prochlorococcus_VI_mg_m3(ind);
        NanoMicroBiomass_int.Synechococcus_VI_mg_m3(i) = PicoBiomass_int.Synechococcus_VI_mg_m3(ind);
    else
        NanoMicroBiomass_int.Prochlorococcus_VI_mg_m3(i) = NaN;
        NanoMicroBiomass_int.Synechococcus_VI_mg_m3(i) = NaN;
    end
end
PhyBiomass = NanoMicroBiomass_int.Prochlorococcus_VI_mg_m3 + NanoMicroBiomass_int.Synechococcus_VI_mg_m3 + NanoMicroBiomass_int.TotalAutoEuk_VI_mg_m3;


fighandle = figure(172);
fighandle.Units = 'inches';
fighandle.Position = [3 1 7 7];
subplot(2,2,1)
plot(NanoMicroBiomass_int.WaterAge,PhyBiomass,'ok','MarkerFaceColor',[0 0.7 0])
ylabel('Total Phytoplankton Biomass (mg C m^-^3)')
xlabel(['Water Parcel Age (days)',char(10),'sensu Chabert et al.'])
set(gca,'FontSize',9)

%Heterotrophic Protists
subplot(2,2,3)
plot(NanoMicroBiomass_int.WaterAge,NanoMicroBiomass_int.TotalHeteroEuk_VI_mg_m3,'ok','MarkerFaceColor',[1 0 1])
ylabel('Heterotrophic Protist Biomass (mg C m^-^3)')
xlabel(['Water Parcel Age (days)',char(10),'sensu Chabert et al.'])
set(gca,'FontSize',9)


%Net Primary Production
subplot(2,2,2)
plot(NPP.WaterAge,NPP.IntC14_mg_m__,'ok','MarkerFaceColor',[0 1 1])
ylabel(['Net Primary Production',char(10),'(mg C m^-^2 / half photoperiod)'])
xlabel(['Water Parcel Age (days)',char(10),'sensu Chabert et al.'])
set(gca,'FontSize',9)

%Metazoan Grazers
Herbivores = renamevars(Doliolids,9,'DoliolidCBiomass_mgCM_2_');
for i=1:height(Doliolids)
    ind=find(strcmp(Doliolids.Cruise(i),Euphausiids.Cruise) & Doliolids.Station(i)==Euphausiids.Station & Doliolids.Line(i)==Euphausiids.Line);
    if length(ind)==1
        Herbivores.EuphausiidsCBiomass_mgCM_2_(i)=Euphausiids.EstimatedCBiomass_mgCM_2_(ind);
    else
        Herbivores.EuphausiidsCBiomass_mgCM_2_(i)=NaN;
    end

    ind=find(strcmp(Doliolids.Cruise(i),Appendicularia.Cruise) & Doliolids.Station(i)==Appendicularia.Station & Doliolids.Line(i)==Appendicularia.Line);
    if length(ind)==1
        Herbivores.AppendiculariaCBiomass_mgCM_2_(i)=Appendicularia.EstimatedCBiomass_mgCM_2_(ind);
    else
        Herbivores.AppendiculariaCBiomass_mgCM_2_(i)=NaN;
    end

    ind=find(strcmp(Doliolids.Cruise(i),Calanoida.Cruise) & Doliolids.Station(i)==Calanoida.Station & Doliolids.Line(i)==Calanoida.Line);
    if length(ind)==1
        Herbivores.CalanoidaCBiomass_mgCM_2_(i)=Calanoida.EstimatedCBiomass_mgCM_2_(ind);
    else
        Herbivores.CalanoidaCBiomass_mgCM_2_(i)=NaN;
    end

    ind=find(strcmp(Doliolids.Cruise(i),Pyrosomes.Cruise) & Doliolids.Station(i)==Pyrosomes.Station & Doliolids.Line(i)==Pyrosomes.Line);
    if length(ind)==1
        Herbivores.PyrosomesCBiomass_mgCM_2_(i)=Pyrosomes.EstimatedCBiomass_mgCM_2_(ind);
    else
        Herbivores.PyrosomesCBiomass_mgCM_2_(i)=NaN;
    end

    ind=find(strcmp(Doliolids.Cruise(i),Salps.Cruise) & Doliolids.Station(i)==Salps.Station & Doliolids.Line(i)==Salps.Line);
    if length(ind)==1
        Herbivores.SalpsCBiomass_mgCM_2_(i)=Salps.EstimatedCBiomass_mgCM_2_(ind);
    else
        Herbivores.SalpsCBiomass_mgCM_2_(i)=NaN;
    end
end

TotHerbivores = Herbivores.DoliolidCBiomass_mgCM_2_ + Herbivores.EuphausiidsCBiomass_mgCM_2_ + Herbivores.CalanoidaCBiomass_mgCM_2_ + Herbivores.PyrosomesCBiomass_mgCM_2_ + Herbivores.SalpsCBiomass_mgCM_2_;
ind = find(strcmp(Herbivores.DayOrNight,'Night'));
subplot(2,2,4)
plot(Herbivores.WaterAge(ind),TotHerbivores(ind),'ok','MarkerFaceColor',[0.6 0.2 0.7])
ylabel(['Herbivorous Metazoan Biomass',char(10),'(mg C m^-^2)'])
xlabel(['Water Parcel Age (days)',char(10),'sensu Chabert et al.'])
set(gca,'FontSize',9)

exportgraphics(gcf,['..\Figures\','StandingStockvWaterAge.png'],'Resolution',600)


origin = 'StandingStockvWaterAge.m';
save('..\Processed Data\OutputPlottedData1.mat','Herbivores','TotHerbivores','NanoMicroBiomass_int','NPP','PhyBiomass','origin')






%____________RECOVERY FIGURE FOR THE %MANUSCRIPT___________________________



fighandle = figure(102);
fighandle.Units = 'inches';
fighandle.Position = [3 1 7 4];

%Net Primary Production
subplot(1,2,1)
plot(NPP.WaterAge,NPP.IntC14_mg_m__,'ok','MarkerFaceColor',[0 1 1])
hold on
ind = find(NPP.WaterAge<100);
output = movaverage(NPP.WaterAge(ind),NPP.IntC14_mg_m__(ind),2);
plot(output(:,1),output(:,2),'r','LineWidth',3)
ylabel(['Net Primary Production',char(10),'(mg C m^-^2 / half photoperiod)'])
xlabel(['Water Parcel Age (days)',char(10),'sensu Chabert et al.'])
set(gca,'FontSize',9)


TotHerbivores = Herbivores.DoliolidCBiomass_mgCM_2_ + Herbivores.EuphausiidsCBiomass_mgCM_2_ + Herbivores.CalanoidaCBiomass_mgCM_2_ + Herbivores.PyrosomesCBiomass_mgCM_2_ + Herbivores.SalpsCBiomass_mgCM_2_;
ind = find(strcmp(Herbivores.DayOrNight,'Night') & Herbivores.WaterAge<100 & isnan(TotHerbivores)==0);
subplot(1,2,2)
plot(Herbivores.WaterAge(ind),TotHerbivores(ind),'ok','MarkerFaceColor',[0.6 0.2 0.7])
hold on
output = movaverage(Herbivores.WaterAge(ind),TotHerbivores(ind),5);
plot(output(:,1),output(:,2),'r','LineWidth',3)
ylabel(['Herbivorous Metazoan Biomass',char(10),'(mg C m^-^2)'])
xlabel(['Water Parcel Age (days)',char(10),'sensu Chabert et al.'])
set(gca,'FontSize',9)
exportgraphics(gcf,['..\Figures\','CCE_Recovery_2023-07-26_StandingStockvWaterAge.png'],'Resolution',600)
