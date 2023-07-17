clearvars
close all

load('..\Processed Data\VertInt & Surface Timeseries.mat')
load('..\Processed Data\TimeSeries.mat')

folder = '..\..\Chabert et al - Water Age Manuscript\NetCDF Files';
load([folder,'/','latlon.mat'])
files = dir(folder);
files(1:2,:)=[];
files(end,:)=[];
for i=1:height(files)
    tmp = files(i).name;
    Y = str2num(tmp(10:13));
    M = str2num(tmp(15:16));
    D = str2num(tmp(18:19));
    ChabertDate(i,1) = datetime(Y,M,D);
end



%NPP
NPP.WaterAge = NaN(height(NPP),1);
for i=1:height(NPP)
    if NPP.Date(i)>ChabertDate(1) & NPP.Date(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-NPP.Date(i))==min(abs(ChabertDate-NPP.Date(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,NPP.Lat_Dec___(i),NPP.Lon_Dec___(i));
        NPP.WaterAge(i)=age;

    else
        NPP.WaterAge(i) = NaN;
    end
end



%PicoBiomass_int
PicoBiomass_int.WaterAge = NaN(height(PicoBiomass_int),1);
for i=1:height(PicoBiomass_int)
    if PicoBiomass_int.DatetimeUTC(i)>ChabertDate(1) & PicoBiomass_int.DatetimeUTC(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-PicoBiomass_int.DatetimeUTC(i))==min(abs(ChabertDate-PicoBiomass_int.DatetimeUTC(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,PicoBiomass_int.Latitude___(i),PicoBiomass_int.Longitude___(i));
        PicoBiomass_int.WaterAge(i)=age;

    else
        PicoBiomass_int.WaterAge(i) = NaN;
    end
end



%NanoMicroBiomass_int
NanoMicroBiomass_int.WaterAge = NaN(height(NanoMicroBiomass_int),1);
for i=1:height(NanoMicroBiomass_int)
    if NanoMicroBiomass_int.DatetimeGMT(i)>ChabertDate(1) & NanoMicroBiomass_int.DatetimeGMT(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-NanoMicroBiomass_int.DatetimeGMT(i))==min(abs(ChabertDate-NanoMicroBiomass_int.DatetimeGMT(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,NanoMicroBiomass_int.Latitude___(i),NanoMicroBiomass_int.Longitude___(i));
        NanoMicroBiomass_int.WaterAge(i)=age;

    else
        NanoMicroBiomass_int.WaterAge(i) = NaN;
    end
end

save('..\Processed Data\VertInt & Surface Timeseries w waterparcel ages.mat','PicoBiomass_int','NanoMicroBiomass_int','NPP')



%SizeFrac
SizeFrac.WaterAge = NaN(height(SizeFrac),1);
for i=1:height(SizeFrac)
    if SizeFrac.DatetimeGMT(i)>ChabertDate(1) & SizeFrac.DatetimeGMT(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-SizeFrac.DatetimeGMT(i))==min(abs(ChabertDate-SizeFrac.DatetimeGMT(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,SizeFrac.Latitude___(i),SizeFrac.Longitude___(i));
        SizeFrac.WaterAge(i)=age;

    else
        SizeFrac.WaterAge(i) = NaN;
    end
end




%Appendicularia
Appendicularia.WaterAge = NaN(height(Appendicularia),1);
for i=1:height(Appendicularia)
    if Appendicularia.StationDate(i)>ChabertDate(1) & Appendicularia.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Appendicularia.StationDate(i))==min(abs(ChabertDate-Appendicularia.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Appendicularia.lat(i),Appendicularia.lon(i));
        Appendicularia.WaterAge(i)=age;

    else
        Appendicularia.WaterAge(i) = NaN;
    end
end


%Calanoida
Calanoida.WaterAge = NaN(height(Calanoida),1);
for i=1:height(Calanoida)
    if Calanoida.StationDate(i)>ChabertDate(1) & Calanoida.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Calanoida.StationDate(i))==min(abs(ChabertDate-Calanoida.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Calanoida.lat(i),Calanoida.lon(i));
        Calanoida.WaterAge(i)=age;

    else
        Calanoida.WaterAge(i) = NaN;
    end
end


%Doliolids
Doliolids.WaterAge = NaN(height(Doliolids),1);
for i=1:height(Doliolids)
    if Doliolids.StationDate(i)>ChabertDate(1) & Doliolids.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Doliolids.StationDate(i))==min(abs(ChabertDate-Doliolids.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Doliolids.lat(i),Doliolids.lon(i));
        Doliolids.WaterAge(i)=age;

    else
        Doliolids.WaterAge(i) = NaN;
    end
end


%Eucalanids
Eucalanids.WaterAge = NaN(height(Eucalanids),1);
for i=1:height(Eucalanids)
    if Eucalanids.StationDate(i)>ChabertDate(1) & Eucalanids.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Eucalanids.StationDate(i))==min(abs(ChabertDate-Eucalanids.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Eucalanids.lat(i),Eucalanids.lon(i));
        Eucalanids.WaterAge(i)=age;

    else
        Eucalanids.WaterAge(i) = NaN;
    end
end


%Euphausiids
Euphausiids.WaterAge = NaN(height(Euphausiids),1);
for i=1:height(Euphausiids)
    if Euphausiids.StationDate(i)>ChabertDate(1) & Euphausiids.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Euphausiids.StationDate(i))==min(abs(ChabertDate-Euphausiids.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Euphausiids.lat(i),Euphausiids.lon(i));
        Euphausiids.WaterAge(i)=age;

    else
        Euphausiids.WaterAge(i) = NaN;
    end
end


%Pyrosomes
Pyrosomes.WaterAge = NaN(height(Pyrosomes),1);
for i=1:height(Pyrosomes)
    if Pyrosomes.StationDate(i)>ChabertDate(1) & Pyrosomes.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Pyrosomes.StationDate(i))==min(abs(ChabertDate-Pyrosomes.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Pyrosomes.lat(i),Pyrosomes.lon(i));
        Pyrosomes.WaterAge(i)=age;

    else
        Pyrosomes.WaterAge(i) = NaN;
    end
end


%Salps
Salps.WaterAge = NaN(height(Salps),1);
for i=1:height(Salps)
    if Salps.StationDate(i)>ChabertDate(1) & Salps.StationDate(i)<ChabertDate(end)
        ind = find(  abs(ChabertDate-Salps.StationDate(i))==min(abs(ChabertDate-Salps.StationDate(i)))   );

        fn = files(ind).name;
        fn = [folder,'/',fn];
        AGE = ncread(fn,'touched');
        AGE(find(AGE==1))=0;  %1 is the marker for land
        AGE(find(AGE==-1))=Inf; %-1 is the marker for age >90 days after which time Chabert did not feel comfortable back-tracking the water parcel
        age = interp2(lat,lon,AGE,Salps.lat(i),Salps.lon(i));
        Salps.WaterAge(i)=age;

    else
        Salps.WaterAge(i) = NaN;
    end
end

save('..\Processed Data\TimeSeries w waterparcel ages.mat','Appendicularia','Calanoida','Doliolids','Eucalanids','Euphausiids','Pyrosomes','Salps','SizeFrac','BEUTI_daily','CUTI_daily','SanDiegoSLA')