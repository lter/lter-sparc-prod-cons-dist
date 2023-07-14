## --------------------------------------------------- ##
            # Download Niwot Data from EDI
## --------------------------------------------------- ##
# Written by: EDI Staff, Nick Lyon, 

## ---------------------------------- ##
# Species Composition ----
## ---------------------------------- ##
# EDI data package URL:
## https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.225.5

# Code taken from EDI code generator (for R)

# Package ID: knb-lter-nwt.225.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Plant species composition in black sand extended growing season experiment, 2018 - ongoing..
# Data set creator:  Jane G Smith -  
# Data set creator:  Jared D Huxley -  
# Data set creator:  Katharine N Suding -  
# Data set creator:  Marko J Spasojevic -  
# Data set creator:  Laurel M Brigham -  
# Data set creator:  Cliff P Bueno de Mesquita -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/225/5/aca4e229e9c1ad1892c34d51c074a5e8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "exp",     
                 "year",     
                 "date_collected",     
                 "monitors",     
                 "local_site",     
                 "treatment",     
                 "transect",     
                 "point",     
                 "hit_number",     
                 "NWT_code",     
                 "USDA_code",     
                 "USDA_name",     
                 "former_ID"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$exp)!="factor") dt1$exp<- as.factor(dt1$exp)
if (class(dt1$year)!="factor") dt1$year<- as.factor(dt1$year)                                   
# attempting to convert dt1$date_collected dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date_collected<-as.Date(dt1$date_collected,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date_collected) == length(tmp1date_collected[!is.na(tmp1date_collected)])){dt1$date_collected <- tmp1date_collected } else {print("Date conversion failed for dt1$date_collected. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date_collected) 
if (class(dt1$monitors)!="factor") dt1$monitors<- as.factor(dt1$monitors)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)
if (class(dt1$treatment)!="factor") dt1$treatment<- as.factor(dt1$treatment)
if (class(dt1$transect)!="factor") dt1$transect<- as.factor(dt1$transect)
if (class(dt1$point)=="factor") dt1$point <-as.numeric(levels(dt1$point))[as.integer(dt1$point) ]               
if (class(dt1$point)=="character") dt1$point <-as.numeric(dt1$point)
if (class(dt1$hit_number)!="factor") dt1$hit_number<- as.factor(dt1$hit_number)
if (class(dt1$NWT_code)!="factor") dt1$NWT_code<- as.factor(dt1$NWT_code)
if (class(dt1$USDA_code)!="factor") dt1$USDA_code<- as.factor(dt1$USDA_code)
if (class(dt1$USDA_name)!="factor") dt1$USDA_name<- as.factor(dt1$USDA_name)
if (class(dt1$former_ID)!="factor") dt1$former_ID<- as.factor(dt1$former_ID)

# Convert Missing Values to NA for non-dates

dt1$former_ID <- as.factor(ifelse((trimws(as.character(dt1$former_ID))==trimws("NaN")),NA,as.character(dt1$former_ID)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(exp)
summary(year)
summary(date_collected)
summary(monitors)
summary(local_site)
summary(treatment)
summary(transect)
summary(point)
summary(hit_number)
summary(NWT_code)
summary(USDA_code)
summary(USDA_name)
summary(former_ID) 
# Get more details on character variables

summary(as.factor(dt1$LTER_site)) 
summary(as.factor(dt1$exp)) 
summary(as.factor(dt1$year)) 
summary(as.factor(dt1$monitors)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$treatment)) 
summary(as.factor(dt1$transect)) 
summary(as.factor(dt1$hit_number)) 
summary(as.factor(dt1$NWT_code)) 
summary(as.factor(dt1$USDA_code)) 
summary(as.factor(dt1$USDA_name)) 
summary(as.factor(dt1$former_ID))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/225/5/0b611023f768e85fb23ad2f6b6479641" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "exp",     
                 "year",     
                 "date_collected",     
                 "monitors",     
                 "local_site",     
                 "treatment",     
                 "subplot",     
                 "X_coord",     
                 "Y_coord",     
                 "hit_number",     
                 "NWT_code",     
                 "USDA_code",     
                 "scientific_name",     
                 "former_ID",     
                 "notes"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$LTER_site)!="factor") dt2$LTER_site<- as.factor(dt2$LTER_site)
if (class(dt2$exp)!="factor") dt2$exp<- as.factor(dt2$exp)
if (class(dt2$year)!="factor") dt2$year<- as.factor(dt2$year)                                   
# attempting to convert dt2$date_collected dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date_collected<-as.Date(dt2$date_collected,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date_collected) == length(tmp2date_collected[!is.na(tmp2date_collected)])){dt2$date_collected <- tmp2date_collected } else {print("Date conversion failed for dt2$date_collected. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date_collected) 
if (class(dt2$monitors)!="factor") dt2$monitors<- as.factor(dt2$monitors)
if (class(dt2$local_site)!="factor") dt2$local_site<- as.factor(dt2$local_site)
if (class(dt2$treatment)!="factor") dt2$treatment<- as.factor(dt2$treatment)
if (class(dt2$subplot)!="factor") dt2$subplot<- as.factor(dt2$subplot)
if (class(dt2$X_coord)!="factor") dt2$X_coord<- as.factor(dt2$X_coord)
if (class(dt2$Y_coord)!="factor") dt2$Y_coord<- as.factor(dt2$Y_coord)
if (class(dt2$hit_number)!="factor") dt2$hit_number<- as.factor(dt2$hit_number)
if (class(dt2$NWT_code)!="factor") dt2$NWT_code<- as.factor(dt2$NWT_code)
if (class(dt2$USDA_code)!="factor") dt2$USDA_code<- as.factor(dt2$USDA_code)
if (class(dt2$scientific_name)!="factor") dt2$scientific_name<- as.factor(dt2$scientific_name)
if (class(dt2$former_ID)!="factor") dt2$former_ID<- as.factor(dt2$former_ID)
if (class(dt2$notes)!="factor") dt2$notes<- as.factor(dt2$notes)

# Convert Missing Values to NA for non-dates

dt2$X_coord <- as.factor(ifelse((trimws(as.character(dt2$X_coord))==trimws("P")),NA,as.character(dt2$X_coord)))
dt2$Y_coord <- as.factor(ifelse((trimws(as.character(dt2$Y_coord))==trimws("P")),NA,as.character(dt2$Y_coord)))
dt2$hit_number <- as.factor(ifelse((trimws(as.character(dt2$hit_number))==trimws("P")),NA,as.character(dt2$hit_number)))
dt2$former_ID <- as.factor(ifelse((trimws(as.character(dt2$former_ID))==trimws("NaN")),NA,as.character(dt2$former_ID)))
dt2$notes <- as.factor(ifelse((trimws(as.character(dt2$notes))==trimws("NaN")),NA,as.character(dt2$notes)))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(exp)
summary(year)
summary(date_collected)
summary(monitors)
summary(local_site)
summary(treatment)
summary(subplot)
summary(X_coord)
summary(Y_coord)
summary(hit_number)
summary(NWT_code)
summary(USDA_code)
summary(scientific_name)
summary(former_ID)
summary(notes) 
# Get more details on character variables

summary(as.factor(dt2$LTER_site)) 
summary(as.factor(dt2$exp)) 
summary(as.factor(dt2$year)) 
summary(as.factor(dt2$monitors)) 
summary(as.factor(dt2$local_site)) 
summary(as.factor(dt2$treatment)) 
summary(as.factor(dt2$subplot)) 
summary(as.factor(dt2$X_coord)) 
summary(as.factor(dt2$Y_coord)) 
summary(as.factor(dt2$hit_number)) 
summary(as.factor(dt2$NWT_code)) 
summary(as.factor(dt2$USDA_code)) 
summary(as.factor(dt2$scientific_name)) 
summary(as.factor(dt2$former_ID)) 
summary(as.factor(dt2$notes))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/225/5/1e96d4e162c2cffca612b1eeb84af61f" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "date_collected",     
                 "monitors",     
                 "site",     
                 "treatment",     
                 "NWT_code",     
                 "USDA_code",     
                 "scientific_name",     
                 "unknowns"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$LTER_site)!="factor") dt3$LTER_site<- as.factor(dt3$LTER_site)                                   
# attempting to convert dt3$date_collected dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3date_collected<-as.Date(dt3$date_collected,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3date_collected) == length(tmp3date_collected[!is.na(tmp3date_collected)])){dt3$date_collected <- tmp3date_collected } else {print("Date conversion failed for dt3$date_collected. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3date_collected) 
if (class(dt3$monitors)!="factor") dt3$monitors<- as.factor(dt3$monitors)
if (class(dt3$site)!="factor") dt3$site<- as.factor(dt3$site)
if (class(dt3$treatment)!="factor") dt3$treatment<- as.factor(dt3$treatment)
if (class(dt3$NWT_code)!="factor") dt3$NWT_code<- as.factor(dt3$NWT_code)
if (class(dt3$USDA_code)!="factor") dt3$USDA_code<- as.factor(dt3$USDA_code)
if (class(dt3$scientific_name)!="factor") dt3$scientific_name<- as.factor(dt3$scientific_name)
if (class(dt3$unknowns)!="factor") dt3$unknowns<- as.factor(dt3$unknowns)

# Convert Missing Values to NA for non-dates

dt3$unknowns <- as.factor(ifelse((trimws(as.character(dt3$unknowns))==trimws("NA")),NA,as.character(dt3$unknowns)))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(date_collected)
summary(monitors)
summary(site)
summary(treatment)
summary(NWT_code)
summary(USDA_code)
summary(scientific_name)
summary(unknowns) 
# Get more details on character variables

summary(as.factor(dt3$LTER_site)) 
summary(as.factor(dt3$monitors)) 
summary(as.factor(dt3$site)) 
summary(as.factor(dt3$treatment)) 
summary(as.factor(dt3$NWT_code)) 
summary(as.factor(dt3$USDA_code)) 
summary(as.factor(dt3$scientific_name)) 
summary(as.factor(dt3$unknowns))
detach(dt3)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/225/5/4b307f2efa64b484c085ff30ed254047" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "date_collected",     
                 "monitors",     
                 "local_site",     
                 "treatment",     
                 "X",     
                 "Y",     
                 "veg_type",     
                 "individual",     
                 "height",     
                 "length",     
                 "width",     
                 "notes"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$LTER_site)!="factor") dt4$LTER_site<- as.factor(dt4$LTER_site)                                   
# attempting to convert dt4$date_collected dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp4date_collected<-as.Date(dt4$date_collected,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp4date_collected) == length(tmp4date_collected[!is.na(tmp4date_collected)])){dt4$date_collected <- tmp4date_collected } else {print("Date conversion failed for dt4$date_collected. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp4date_collected) 
if (class(dt4$monitors)!="factor") dt4$monitors<- as.factor(dt4$monitors)
if (class(dt4$local_site)!="factor") dt4$local_site<- as.factor(dt4$local_site)
if (class(dt4$treatment)!="factor") dt4$treatment<- as.factor(dt4$treatment)
if (class(dt4$X)=="factor") dt4$X <-as.numeric(levels(dt4$X))[as.integer(dt4$X) ]               
if (class(dt4$X)=="character") dt4$X <-as.numeric(dt4$X)
if (class(dt4$Y)=="factor") dt4$Y <-as.numeric(levels(dt4$Y))[as.integer(dt4$Y) ]               
if (class(dt4$Y)=="character") dt4$Y <-as.numeric(dt4$Y)
if (class(dt4$veg_type)!="factor") dt4$veg_type<- as.factor(dt4$veg_type)
if (class(dt4$individual)!="factor") dt4$individual<- as.factor(dt4$individual)
if (class(dt4$height)=="factor") dt4$height <-as.numeric(levels(dt4$height))[as.integer(dt4$height) ]               
if (class(dt4$height)=="character") dt4$height <-as.numeric(dt4$height)
if (class(dt4$length)=="factor") dt4$length <-as.numeric(levels(dt4$length))[as.integer(dt4$length) ]               
if (class(dt4$length)=="character") dt4$length <-as.numeric(dt4$length)
if (class(dt4$width)=="factor") dt4$width <-as.numeric(levels(dt4$width))[as.integer(dt4$width) ]               
if (class(dt4$width)=="character") dt4$width <-as.numeric(dt4$width)
if (class(dt4$notes)!="factor") dt4$notes<- as.factor(dt4$notes)

# Convert Missing Values to NA for non-dates

dt4$X <- ifelse((trimws(as.character(dt4$X))==trimws("NaN")),NA,dt4$X)               
suppressWarnings(dt4$X <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt4$X))==as.character(as.numeric("NaN"))),NA,dt4$X))
dt4$Y <- ifelse((trimws(as.character(dt4$Y))==trimws("NaN")),NA,dt4$Y)               
suppressWarnings(dt4$Y <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt4$Y))==as.character(as.numeric("NaN"))),NA,dt4$Y))
dt4$height <- ifelse((trimws(as.character(dt4$height))==trimws("NaN")),NA,dt4$height)               
suppressWarnings(dt4$height <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt4$height))==as.character(as.numeric("NaN"))),NA,dt4$height))
dt4$length <- ifelse((trimws(as.character(dt4$length))==trimws("NaN")),NA,dt4$length)               
suppressWarnings(dt4$length <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt4$length))==as.character(as.numeric("NaN"))),NA,dt4$length))
dt4$width <- ifelse((trimws(as.character(dt4$width))==trimws("NaN")),NA,dt4$width)               
suppressWarnings(dt4$width <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt4$width))==as.character(as.numeric("NaN"))),NA,dt4$width))
dt4$notes <- as.factor(ifelse((trimws(as.character(dt4$notes))==trimws("NaN")),NA,as.character(dt4$notes)))


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(date_collected)
summary(monitors)
summary(local_site)
summary(treatment)
summary(X)
summary(Y)
summary(veg_type)
summary(individual)
summary(height)
summary(length)
summary(width)
summary(notes) 
# Get more details on character variables

summary(as.factor(dt4$LTER_site)) 
summary(as.factor(dt4$monitors)) 
summary(as.factor(dt4$local_site)) 
summary(as.factor(dt4$treatment)) 
summary(as.factor(dt4$veg_type)) 
summary(as.factor(dt4$individual)) 
summary(as.factor(dt4$notes))
detach(dt4)               


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/225/5/a96cbafa12ee2eb080d4d2dd329e1619" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "date_collected",     
                 "monitors",     
                 "local_site",     
                 "treatment",     
                 "X",     
                 "Y",     
                 "veg_type",     
                 "individual",     
                 "diameter",     
                 "height"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$LTER_site)!="factor") dt5$LTER_site<- as.factor(dt5$LTER_site)                                   
# attempting to convert dt5$date_collected dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp5date_collected<-as.Date(dt5$date_collected,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp5date_collected) == length(tmp5date_collected[!is.na(tmp5date_collected)])){dt5$date_collected <- tmp5date_collected } else {print("Date conversion failed for dt5$date_collected. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp5date_collected) 
if (class(dt5$monitors)!="factor") dt5$monitors<- as.factor(dt5$monitors)
if (class(dt5$local_site)!="factor") dt5$local_site<- as.factor(dt5$local_site)
if (class(dt5$treatment)!="factor") dt5$treatment<- as.factor(dt5$treatment)
if (class(dt5$X)=="factor") dt5$X <-as.numeric(levels(dt5$X))[as.integer(dt5$X) ]               
if (class(dt5$X)=="character") dt5$X <-as.numeric(dt5$X)
if (class(dt5$Y)=="factor") dt5$Y <-as.numeric(levels(dt5$Y))[as.integer(dt5$Y) ]               
if (class(dt5$Y)=="character") dt5$Y <-as.numeric(dt5$Y)
if (class(dt5$veg_type)!="factor") dt5$veg_type<- as.factor(dt5$veg_type)
if (class(dt5$individual)!="factor") dt5$individual<- as.factor(dt5$individual)
if (class(dt5$diameter)=="factor") dt5$diameter <-as.numeric(levels(dt5$diameter))[as.integer(dt5$diameter) ]               
if (class(dt5$diameter)=="character") dt5$diameter <-as.numeric(dt5$diameter)
if (class(dt5$height)=="factor") dt5$height <-as.numeric(levels(dt5$height))[as.integer(dt5$height) ]               
if (class(dt5$height)=="character") dt5$height <-as.numeric(dt5$height)

# Convert Missing Values to NA for non-dates

dt5$X <- ifelse((trimws(as.character(dt5$X))==trimws("NaN")),NA,dt5$X)               
suppressWarnings(dt5$X <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt5$X))==as.character(as.numeric("NaN"))),NA,dt5$X))
dt5$Y <- ifelse((trimws(as.character(dt5$Y))==trimws("NaN")),NA,dt5$Y)               
suppressWarnings(dt5$Y <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt5$Y))==as.character(as.numeric("NaN"))),NA,dt5$Y))
dt5$diameter <- ifelse((trimws(as.character(dt5$diameter))==trimws("NaN")),NA,dt5$diameter)               
suppressWarnings(dt5$diameter <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt5$diameter))==as.character(as.numeric("NaN"))),NA,dt5$diameter))


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(date_collected)
summary(monitors)
summary(local_site)
summary(treatment)
summary(X)
summary(Y)
summary(veg_type)
summary(individual)
summary(diameter)
summary(height) 
# Get more details on character variables

summary(as.factor(dt5$LTER_site)) 
summary(as.factor(dt5$monitors)) 
summary(as.factor(dt5$local_site)) 
summary(as.factor(dt5$treatment)) 
summary(as.factor(dt5$veg_type)) 
summary(as.factor(dt5$individual))
detach(dt5)               

## ---------------------------------- ##
        # Export Composition ----
## ---------------------------------- ##

# Export these locally
## Species Composition (transect)
write.csv(x = dt1, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "spp-comp-transect.csv"))

## Species Composition (subplot)
write.csv(x = dt2, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "spp-comp-subplot.csv"))

## Species Composition (whole plot)
write.csv(x = dt3, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "spp-comp-whole-plot.csv"))

## Woody Species Demography
write.csv(x = dt4, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "demography-woody-spp.csv"))

## Krummholz Demography
write.csv(x = dt5, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "demography-krumm.csv"))

## ---------------------------------- ##
    # Composition Housekeeping ----
## ---------------------------------- ##

# Clear environment (reduces errors due to object name re-use)
rm(list = ls())

## ---------------------------------- ##
      # Soil Characteristics ----
## ---------------------------------- ##
# EDI data package URL:
## https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.238.4

# Package ID: knb-lter-nwt.238.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Soil moisture, temperature, and electrical conductivity data from the black sand extended growing season length experiment, 2018 - ongoing, hourly..
# Data set creator:  Jennifer F Morse -  
# Data set creator:    - Niwot Ridge LTER 
# Contact:    - Information Manager Niwot Ridge LTER  - lternwt@colorado.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/238/4/b36aceff9dfcd5be5c98e47e21470f23" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "LTER_site",     
                 "local_site",     
                 "date",     
                 "soiltemp5_5cm_avg",     
                 "flag_soiltemp5_5cm_avg",     
                 "ec5_5cm_avg",     
                 "flag_ec5_5cm_avg",     
                 "soilmoisture5_5cm_avg",     
                 "flag_soilmoisture5_5cm_avg",     
                 "soiltemp6_5cm_avg",     
                 "flag_soiltemp6_5cm_avg",     
                 "ec6_5cm_avg",     
                 "flag_ec6_5cm_avg",     
                 "soilmoisture6_5cm_avg",     
                 "flag_soilmoisture6_5cm_avg",     
                 "soiltemp7_5cm_avg",     
                 "flag_soiltemp7_5cm_avg",     
                 "ec7_5cm_avg",     
                 "flag_ec7_5cm_avg",     
                 "soilmoisture7_5cm_avg",     
                 "flag_soilmoisture7_5cm_avg",     
                 "soiltemp8_5cm_avg",     
                 "flag_soiltemp8_5cm_avg",     
                 "ec8_5cm_avg",     
                 "flag_ec8_5cm_avg",     
                 "soilmoisture8_5cm_avg",     
                 "flag_soilmoisture8_5cm_avg",     
                 "batt_volt_min",     
                 "flag_batt_volt_min"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1date<-as.POSIXct(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$soiltemp5_5cm_avg)=="factor") dt1$soiltemp5_5cm_avg <-as.numeric(levels(dt1$soiltemp5_5cm_avg))[as.integer(dt1$soiltemp5_5cm_avg) ]               
if (class(dt1$soiltemp5_5cm_avg)=="character") dt1$soiltemp5_5cm_avg <-as.numeric(dt1$soiltemp5_5cm_avg)
if (class(dt1$flag_soiltemp5_5cm_avg)!="factor") dt1$flag_soiltemp5_5cm_avg<- as.factor(dt1$flag_soiltemp5_5cm_avg)
if (class(dt1$ec5_5cm_avg)=="factor") dt1$ec5_5cm_avg <-as.numeric(levels(dt1$ec5_5cm_avg))[as.integer(dt1$ec5_5cm_avg) ]               
if (class(dt1$ec5_5cm_avg)=="character") dt1$ec5_5cm_avg <-as.numeric(dt1$ec5_5cm_avg)
if (class(dt1$flag_ec5_5cm_avg)!="factor") dt1$flag_ec5_5cm_avg<- as.factor(dt1$flag_ec5_5cm_avg)
if (class(dt1$soilmoisture5_5cm_avg)=="factor") dt1$soilmoisture5_5cm_avg <-as.numeric(levels(dt1$soilmoisture5_5cm_avg))[as.integer(dt1$soilmoisture5_5cm_avg) ]               
if (class(dt1$soilmoisture5_5cm_avg)=="character") dt1$soilmoisture5_5cm_avg <-as.numeric(dt1$soilmoisture5_5cm_avg)
if (class(dt1$flag_soilmoisture5_5cm_avg)!="factor") dt1$flag_soilmoisture5_5cm_avg<- as.factor(dt1$flag_soilmoisture5_5cm_avg)
if (class(dt1$soiltemp6_5cm_avg)=="factor") dt1$soiltemp6_5cm_avg <-as.numeric(levels(dt1$soiltemp6_5cm_avg))[as.integer(dt1$soiltemp6_5cm_avg) ]               
if (class(dt1$soiltemp6_5cm_avg)=="character") dt1$soiltemp6_5cm_avg <-as.numeric(dt1$soiltemp6_5cm_avg)
if (class(dt1$flag_soiltemp6_5cm_avg)!="factor") dt1$flag_soiltemp6_5cm_avg<- as.factor(dt1$flag_soiltemp6_5cm_avg)
if (class(dt1$ec6_5cm_avg)=="factor") dt1$ec6_5cm_avg <-as.numeric(levels(dt1$ec6_5cm_avg))[as.integer(dt1$ec6_5cm_avg) ]               
if (class(dt1$ec6_5cm_avg)=="character") dt1$ec6_5cm_avg <-as.numeric(dt1$ec6_5cm_avg)
if (class(dt1$flag_ec6_5cm_avg)!="factor") dt1$flag_ec6_5cm_avg<- as.factor(dt1$flag_ec6_5cm_avg)
if (class(dt1$soilmoisture6_5cm_avg)=="factor") dt1$soilmoisture6_5cm_avg <-as.numeric(levels(dt1$soilmoisture6_5cm_avg))[as.integer(dt1$soilmoisture6_5cm_avg) ]               
if (class(dt1$soilmoisture6_5cm_avg)=="character") dt1$soilmoisture6_5cm_avg <-as.numeric(dt1$soilmoisture6_5cm_avg)
if (class(dt1$flag_soilmoisture6_5cm_avg)!="factor") dt1$flag_soilmoisture6_5cm_avg<- as.factor(dt1$flag_soilmoisture6_5cm_avg)
if (class(dt1$soiltemp7_5cm_avg)=="factor") dt1$soiltemp7_5cm_avg <-as.numeric(levels(dt1$soiltemp7_5cm_avg))[as.integer(dt1$soiltemp7_5cm_avg) ]               
if (class(dt1$soiltemp7_5cm_avg)=="character") dt1$soiltemp7_5cm_avg <-as.numeric(dt1$soiltemp7_5cm_avg)
if (class(dt1$flag_soiltemp7_5cm_avg)!="factor") dt1$flag_soiltemp7_5cm_avg<- as.factor(dt1$flag_soiltemp7_5cm_avg)
if (class(dt1$ec7_5cm_avg)=="factor") dt1$ec7_5cm_avg <-as.numeric(levels(dt1$ec7_5cm_avg))[as.integer(dt1$ec7_5cm_avg) ]               
if (class(dt1$ec7_5cm_avg)=="character") dt1$ec7_5cm_avg <-as.numeric(dt1$ec7_5cm_avg)
if (class(dt1$flag_ec7_5cm_avg)!="factor") dt1$flag_ec7_5cm_avg<- as.factor(dt1$flag_ec7_5cm_avg)
if (class(dt1$soilmoisture7_5cm_avg)=="factor") dt1$soilmoisture7_5cm_avg <-as.numeric(levels(dt1$soilmoisture7_5cm_avg))[as.integer(dt1$soilmoisture7_5cm_avg) ]               
if (class(dt1$soilmoisture7_5cm_avg)=="character") dt1$soilmoisture7_5cm_avg <-as.numeric(dt1$soilmoisture7_5cm_avg)
if (class(dt1$flag_soilmoisture7_5cm_avg)!="factor") dt1$flag_soilmoisture7_5cm_avg<- as.factor(dt1$flag_soilmoisture7_5cm_avg)
if (class(dt1$soiltemp8_5cm_avg)=="factor") dt1$soiltemp8_5cm_avg <-as.numeric(levels(dt1$soiltemp8_5cm_avg))[as.integer(dt1$soiltemp8_5cm_avg) ]               
if (class(dt1$soiltemp8_5cm_avg)=="character") dt1$soiltemp8_5cm_avg <-as.numeric(dt1$soiltemp8_5cm_avg)
if (class(dt1$flag_soiltemp8_5cm_avg)!="factor") dt1$flag_soiltemp8_5cm_avg<- as.factor(dt1$flag_soiltemp8_5cm_avg)
if (class(dt1$ec8_5cm_avg)=="factor") dt1$ec8_5cm_avg <-as.numeric(levels(dt1$ec8_5cm_avg))[as.integer(dt1$ec8_5cm_avg) ]               
if (class(dt1$ec8_5cm_avg)=="character") dt1$ec8_5cm_avg <-as.numeric(dt1$ec8_5cm_avg)
if (class(dt1$flag_ec8_5cm_avg)!="factor") dt1$flag_ec8_5cm_avg<- as.factor(dt1$flag_ec8_5cm_avg)
if (class(dt1$soilmoisture8_5cm_avg)=="factor") dt1$soilmoisture8_5cm_avg <-as.numeric(levels(dt1$soilmoisture8_5cm_avg))[as.integer(dt1$soilmoisture8_5cm_avg) ]               
if (class(dt1$soilmoisture8_5cm_avg)=="character") dt1$soilmoisture8_5cm_avg <-as.numeric(dt1$soilmoisture8_5cm_avg)
if (class(dt1$flag_soilmoisture8_5cm_avg)!="factor") dt1$flag_soilmoisture8_5cm_avg<- as.factor(dt1$flag_soilmoisture8_5cm_avg)
if (class(dt1$batt_volt_min)=="factor") dt1$batt_volt_min <-as.numeric(levels(dt1$batt_volt_min))[as.integer(dt1$batt_volt_min) ]               
if (class(dt1$batt_volt_min)=="character") dt1$batt_volt_min <-as.numeric(dt1$batt_volt_min)
if (class(dt1$flag_batt_volt_min)!="factor") dt1$flag_batt_volt_min<- as.factor(dt1$flag_batt_volt_min)

# Convert Missing Values to NA for non-dates

dt1$soiltemp5_5cm_avg <- ifelse((trimws(as.character(dt1$soiltemp5_5cm_avg))==trimws("NaN")),NA,dt1$soiltemp5_5cm_avg)               
suppressWarnings(dt1$soiltemp5_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soiltemp5_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soiltemp5_5cm_avg))
dt1$flag_soiltemp5_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soiltemp5_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soiltemp5_5cm_avg)))
dt1$ec5_5cm_avg <- ifelse((trimws(as.character(dt1$ec5_5cm_avg))==trimws("NaN")),NA,dt1$ec5_5cm_avg)               
suppressWarnings(dt1$ec5_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$ec5_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$ec5_5cm_avg))
dt1$flag_ec5_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_ec5_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_ec5_5cm_avg)))
dt1$soilmoisture5_5cm_avg <- ifelse((trimws(as.character(dt1$soilmoisture5_5cm_avg))==trimws("NaN")),NA,dt1$soilmoisture5_5cm_avg)               
suppressWarnings(dt1$soilmoisture5_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soilmoisture5_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soilmoisture5_5cm_avg))
dt1$flag_soilmoisture5_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soilmoisture5_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soilmoisture5_5cm_avg)))
dt1$soiltemp6_5cm_avg <- ifelse((trimws(as.character(dt1$soiltemp6_5cm_avg))==trimws("NaN")),NA,dt1$soiltemp6_5cm_avg)               
suppressWarnings(dt1$soiltemp6_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soiltemp6_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soiltemp6_5cm_avg))
dt1$flag_soiltemp6_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soiltemp6_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soiltemp6_5cm_avg)))
dt1$ec6_5cm_avg <- ifelse((trimws(as.character(dt1$ec6_5cm_avg))==trimws("NaN")),NA,dt1$ec6_5cm_avg)               
suppressWarnings(dt1$ec6_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$ec6_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$ec6_5cm_avg))
dt1$flag_ec6_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_ec6_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_ec6_5cm_avg)))
dt1$soilmoisture6_5cm_avg <- ifelse((trimws(as.character(dt1$soilmoisture6_5cm_avg))==trimws("NaN")),NA,dt1$soilmoisture6_5cm_avg)               
suppressWarnings(dt1$soilmoisture6_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soilmoisture6_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soilmoisture6_5cm_avg))
dt1$flag_soilmoisture6_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soilmoisture6_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soilmoisture6_5cm_avg)))
dt1$soiltemp7_5cm_avg <- ifelse((trimws(as.character(dt1$soiltemp7_5cm_avg))==trimws("NaN")),NA,dt1$soiltemp7_5cm_avg)               
suppressWarnings(dt1$soiltemp7_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soiltemp7_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soiltemp7_5cm_avg))
dt1$flag_soiltemp7_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soiltemp7_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soiltemp7_5cm_avg)))
dt1$ec7_5cm_avg <- ifelse((trimws(as.character(dt1$ec7_5cm_avg))==trimws("NaN")),NA,dt1$ec7_5cm_avg)               
suppressWarnings(dt1$ec7_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$ec7_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$ec7_5cm_avg))
dt1$flag_ec7_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_ec7_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_ec7_5cm_avg)))
dt1$soilmoisture7_5cm_avg <- ifelse((trimws(as.character(dt1$soilmoisture7_5cm_avg))==trimws("NaN")),NA,dt1$soilmoisture7_5cm_avg)               
suppressWarnings(dt1$soilmoisture7_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soilmoisture7_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soilmoisture7_5cm_avg))
dt1$flag_soilmoisture7_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soilmoisture7_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soilmoisture7_5cm_avg)))
dt1$soiltemp8_5cm_avg <- ifelse((trimws(as.character(dt1$soiltemp8_5cm_avg))==trimws("NaN")),NA,dt1$soiltemp8_5cm_avg)               
suppressWarnings(dt1$soiltemp8_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soiltemp8_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soiltemp8_5cm_avg))
dt1$flag_soiltemp8_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soiltemp8_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soiltemp8_5cm_avg)))
dt1$ec8_5cm_avg <- ifelse((trimws(as.character(dt1$ec8_5cm_avg))==trimws("NaN")),NA,dt1$ec8_5cm_avg)               
suppressWarnings(dt1$ec8_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$ec8_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$ec8_5cm_avg))
dt1$flag_ec8_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_ec8_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_ec8_5cm_avg)))
dt1$soilmoisture8_5cm_avg <- ifelse((trimws(as.character(dt1$soilmoisture8_5cm_avg))==trimws("NaN")),NA,dt1$soilmoisture8_5cm_avg)               
suppressWarnings(dt1$soilmoisture8_5cm_avg <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$soilmoisture8_5cm_avg))==as.character(as.numeric("NaN"))),NA,dt1$soilmoisture8_5cm_avg))
dt1$flag_soilmoisture8_5cm_avg <- as.factor(ifelse((trimws(as.character(dt1$flag_soilmoisture8_5cm_avg))==trimws("NaN")),NA,as.character(dt1$flag_soilmoisture8_5cm_avg)))
dt1$batt_volt_min <- ifelse((trimws(as.character(dt1$batt_volt_min))==trimws("NaN")),NA,dt1$batt_volt_min)               
suppressWarnings(dt1$batt_volt_min <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$batt_volt_min))==as.character(as.numeric("NaN"))),NA,dt1$batt_volt_min))
dt1$flag_batt_volt_min <- as.factor(ifelse((trimws(as.character(dt1$flag_batt_volt_min))==trimws("NaN")),NA,as.character(dt1$flag_batt_volt_min)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LTER_site)
summary(local_site)
summary(date)
summary(soiltemp5_5cm_avg)
summary(flag_soiltemp5_5cm_avg)
summary(ec5_5cm_avg)
summary(flag_ec5_5cm_avg)
summary(soilmoisture5_5cm_avg)
summary(flag_soilmoisture5_5cm_avg)
summary(soiltemp6_5cm_avg)
summary(flag_soiltemp6_5cm_avg)
summary(ec6_5cm_avg)
summary(flag_ec6_5cm_avg)
summary(soilmoisture6_5cm_avg)
summary(flag_soilmoisture6_5cm_avg)
summary(soiltemp7_5cm_avg)
summary(flag_soiltemp7_5cm_avg)
summary(ec7_5cm_avg)
summary(flag_ec7_5cm_avg)
summary(soilmoisture7_5cm_avg)
summary(flag_soilmoisture7_5cm_avg)
summary(soiltemp8_5cm_avg)
summary(flag_soiltemp8_5cm_avg)
summary(ec8_5cm_avg)
summary(flag_ec8_5cm_avg)
summary(soilmoisture8_5cm_avg)
summary(flag_soilmoisture8_5cm_avg)
summary(batt_volt_min)
summary(flag_batt_volt_min) 
# Get more details on character variables

summary(as.factor(dt1$LTER_site)) 
summary(as.factor(dt1$local_site)) 
summary(as.factor(dt1$flag_soiltemp5_5cm_avg)) 
summary(as.factor(dt1$flag_ec5_5cm_avg)) 
summary(as.factor(dt1$flag_soilmoisture5_5cm_avg)) 
summary(as.factor(dt1$flag_soiltemp6_5cm_avg)) 
summary(as.factor(dt1$flag_ec6_5cm_avg)) 
summary(as.factor(dt1$flag_soilmoisture6_5cm_avg)) 
summary(as.factor(dt1$flag_soiltemp7_5cm_avg)) 
summary(as.factor(dt1$flag_ec7_5cm_avg)) 
summary(as.factor(dt1$flag_soilmoisture7_5cm_avg)) 
summary(as.factor(dt1$flag_soiltemp8_5cm_avg)) 
summary(as.factor(dt1$flag_ec8_5cm_avg)) 
summary(as.factor(dt1$flag_soilmoisture8_5cm_avg)) 
summary(as.factor(dt1$flag_batt_volt_min))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/238/4/024605e81d0308609981d83268388934" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sensor",     
                 "latitude",     
                 "longitude",     
                 "elevation",     
                 "distance_from_lower_edge_of_plot"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$sensor)!="factor") dt2$sensor<- as.factor(dt2$sensor)
if (class(dt2$latitude)=="factor") dt2$latitude <-as.numeric(levels(dt2$latitude))[as.integer(dt2$latitude) ]               
if (class(dt2$latitude)=="character") dt2$latitude <-as.numeric(dt2$latitude)
if (class(dt2$longitude)=="factor") dt2$longitude <-as.numeric(levels(dt2$longitude))[as.integer(dt2$longitude) ]               
if (class(dt2$longitude)=="character") dt2$longitude <-as.numeric(dt2$longitude)
if (class(dt2$elevation)=="factor") dt2$elevation <-as.numeric(levels(dt2$elevation))[as.integer(dt2$elevation) ]               
if (class(dt2$elevation)=="character") dt2$elevation <-as.numeric(dt2$elevation)
if (class(dt2$distance_from_lower_edge_of_plot)=="factor") dt2$distance_from_lower_edge_of_plot <-as.numeric(levels(dt2$distance_from_lower_edge_of_plot))[as.integer(dt2$distance_from_lower_edge_of_plot) ]               
if (class(dt2$distance_from_lower_edge_of_plot)=="character") dt2$distance_from_lower_edge_of_plot <-as.numeric(dt2$distance_from_lower_edge_of_plot)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sensor)
summary(latitude)
summary(longitude)
summary(elevation)
summary(distance_from_lower_edge_of_plot) 
# Get more details on character variables

summary(as.factor(dt2$sensor))
detach(dt2)               

## ---------------------------------- ##
      # Export Characteristics ----
## ---------------------------------- ##

# Export these locally
## Soil Characteristics (data)
write.csv(x = dt1, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "soil-sensor-data.csv"))

## Soil Characteristics (sensor locations)
write.csv(x = dt2, row.names = F, na = '',
          file = file.path("LTER-NWT-WarrenS24", "soil-sensor-loc.csv"))

## ---------------------------------- ##
  # Characteristic Housekeeping ----
## ---------------------------------- ##

# Clear environment (reduces errors due to object name re-use)
rm(list = ls())

# End ----
