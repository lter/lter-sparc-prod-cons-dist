## ---------------------------
##
## Script name: ntl-prod-cons-troutlake.R
##
## Purpose of script: munging data from spiny invasion in Trout for SPARC project
##
## Author: Grace Wilkinson
##
## Email: gwilkinson@wisc.edu
##
## ------------------------

library(tidyverse)
library(lubridate)


# Package ID: knb-lter-ntl.1.59 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/59/0ff1fd13116d6097376e3745194cdc5f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F ,skip=1, sep=",", quot='"', col.names=c(
  "lakeid", "year4", "daynum", "sampledate", "depth", "rep", "sta",
  "event", "ph", "phair", "alk", "dic", "tic", "doc", "toc",     
  "no3no2", "no2", "nh4", "totnf","totnuf","totpf", "totpuf",     
  "drsif", "brsif", "brsiuf","tpm","totnuf_sloh","no3no2_sloh",     
  "nh4_sloh","kjdl_n_sloh","totpuf_sloh","drp_sloh", "drsif_sloh", 
  "flagdepth", "flagph", "flagphair","flagalk", "flagdic","flagtic",
  "flagdoc","flagtoc","flagno3no2", "flagno2","flagnh4","flagtotnf",
  "flagtotnuf","flagtotpf","flagtotpuf","flagdrsif", "flagbrsif",  
  "flagbrsiuf","flagtpm", "flagtotnuf_sloh","flagno3no2_sloh",     
  "flagnh4_sloh","flagkjdl_n_sloh",     
  "flagtotpuf_sloh", "flagdrp_sloh", "flagdrsif_sloh"), 
  check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
chem = dt1 %>%
  select(lakeid, year4, daynum, sampledate, depth, 
         totnf, totnuf, totpf, totpuf) %>%
  filter(lakeid=="TR") %>%
  filter(year4>=2007) %>%
  filter(depth<=5) %>%
  mutate(month = month(sampledate)) %>%
  filter(month>=4) %>%
  filter(month<=10) %>%
  select(-lakeid, -daynum) %>%
  filter(!depth==3)

chem_wide = pivot_wider(chem, 
                        id_cols = c(year4, sampledate),
                        names_from = depth, 
                        values_from = c(totnf, totnuf, totpf, totpuf))

# Package ID: knb-lter-ntl.35.32 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chlorophyll - Trout Lake Area 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/35/32/50f9b5f93d0a0d47008147698fb413f3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

chl <-read.csv(infile1,header=F,skip=1,sep=",",quot='"',
               col.names=c("lakeid", "year4", "daynum","sampledate", "rep", "sta", "depth",     
                           "chlor", "phaeo","flagchlor", "flagphaeo"), check.names=TRUE)

unlink(infile1)

# attempting to convert chl$sampledate dateTime string to R date structure (date or POSIXct)    
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(chl$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){chl$sampledate <- tmp1sampledate } else {print("Date conversion failed for chl$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

chlorophyll = chl %>% 
  filter(lakeid=='TR') %>% 
  filter(year4>=2007) %>%
  filter(depth<=5) %>%
  mutate(month = month(sampledate)) %>%
  filter(month>=4) %>%
  filter(month<=10) %>%
  select(-rep, -sta, -phaeo, -flagphaeo, -lakeid)

chl_wide = pivot_wider(chlorophyll, 
                       id_cols = c(year4, sampledate),
                       names_from = depth, 
                       names_prefix = 'chl',
                       values_from = chlor) %>%
  mutate(chl_surface = case_when(is.na(.$chl0) ~ chl1, 
                                 TRUE ~ chl0)) %>%
  select(-chl0, -chl1)

chem_chl_merge = left_join(chl_wide, chem_wide, by = c('year4', 'sampledate'))

# Package ID: knb-lter-ntl.37.37 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Zooplankton - Trout Lake Area 1982 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/37/c4b652eea76cd431ac5fd3562b1837ee" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

zoops <-read.csv(infile1,header=F,skip=1,sep=",",quot='"', col.names=c(
  "lakeid","year4","sample_date","station","species_code",     
  "species_name","density","individuals_measured","avg_length"), check.names=TRUE)

unlink(infile1)

# attempting to convert zoops$sample_date dateTime string to R date structure (date or POSIXct)    
tmpDateFormat<-"%Y-%m-%d"
tmp1sample_date<-as.Date(zoops$sample_date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sample_date) == length(tmp1sample_date[!is.na(tmp1sample_date)])){zoops$sample_date <- tmp1sample_date } else {print("Date conversion failed for zoops$sample_date. Please inspect the data and do the date conversion yourself.")}                                                       
# Let's start tidying =================
trout_zoops = zoops %>% 
  filter(lakeid=='TR') %>%
  filter(year4>=2007) %>%
  # Get rid of unnecessary columns
  select(-lakeid, -station, -species_code) %>%
  mutate(month = month(sample_date)) %>%
  # Filter to ice-free season
  filter(month>=5) %>%
  filter(month<=10)

#create a file of all species to assign to taxonomic groups
#from this species listing, assigned taxonomic groupings to zoops so that I'm not dealing with 45 rotifer names
# write.csv(unique(trout_zoops$species_name), file = 'Trout Zoop Species.csv')

#read the species grouping file back in merge with data frame
groups = read.csv("Trout Zoop Species.csv") %>%
  rename(species_name = species)
#Merge the two data frames
trout_zoops = left_join(trout_zoops, groups, by = c("species_name"))

#Filter down to just the Daphnia - might be the only 
tr_daphnia = trout_zoops %>%
  filter(group == 'cladoceran') %>%
  select(-individuals_measured)

# Calculate the mean length by species
clado_length = tr_daphnia %>%
  group_by(species_name) %>%
  summarize(global_spp_length = mean(avg_length, na.rm = TRUE)) %>%
  ungroup()

# Create a new column with the global mean species length
tr_daphnia1 = left_join(tr_daphnia, clado_length, by = 'species_name')

# Sometimes they don't measure lengths, so we'll use the average for that species 
# if a length measurement is missing. Not perfect, but it's what we have...
# Could try to make it month-specific for lengths later to get a better phenology
tr_daphnia = tr_daphnia1 %>%
  mutate(avg_length = case_when(is.na(.$avg_length) ~ global_spp_length, 
                      TRUE ~ avg_length))

# Now we need to calculate biomass. Get ready for an epic case_when()
tr_biomass = tr_daphnia %>%
  mutate(biomass = case_when(
    .$biomass_taxa == 'bosmina'~ (26.6*(avg_length)^3.13)*density,
    .$biomass_taxa == 'daphnia'~ ((1.5*10^-8)*(avg_length)^2.84)*density,
    .$biomass_taxa == 'ceriodaphnia'~ ((1.76*10^-6)*(avg_length)^2.26)*density,
    .$biomass_taxa == 'chydorus'~ (89.43*(avg_length)^3.03)*density)) %>%
  # sum all cladoceran biomass by sampling date
  group_by(year4, sample_date) %>%
  summarize(cladoceran_biomass = sum(biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(sampledate = sample_date)

# # Pivot the data frame wide
# zoops_wide = pivot_wider(total_zoops, 
#                         id_cols = c(year4, sample_date), 
#                         names_from = group, 
#                         values_from = c(tot_zoops)) %>%
#   rename(sampledate = sample_date)


# MERGE THE DATA FRAMES ===================================
ntl_prod_cons = left_join(chem_chl_merge, tr_biomass,
                          by = c("sampledate", "year4")) %>%
  mutate(chl_epi = mean(c(chl_surface, chl3, chl5), na.rm = TRUE),
         tdn_epi = mean(c(totnf_0, totnf_5), na.rm = TRUE),
         tn_epi = mean(c(totnuf_0, totnuf_5), na.rm = TRUE),
         tdp_epi = mean(c(totpf_0, totpf_5), na.rm = TRUE),
         tp_epi = mean(c(totpuf_0, totpuf_5), na.rm = TRUE)) %>%
  select(-chl3:-totpuf_5)


# Create a column for the disturbance event
ntl_prod_cons$spiny_dist = NA
ntl_trout = ntl_prod_cons %>%
  mutate(spiny_dist = case_when(year4 < 2014 ~ "pre",
                                year4 >= 2014 ~ "post"))

#Save the file
write.csv(ntl_trout, file = "NTL_TroutLake_SpinyWaterFlea.csv")
