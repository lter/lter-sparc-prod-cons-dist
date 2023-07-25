## ---------------------------
## Script name: MCR_Producer_Consumer.R
## Purpose of script: cleaning data for SPARC project
## Author: Noe Castaneda
## Email: ncastaneda@ucsb.edu
## ------------------------

library(tidyverse)
library(lubridate)

# Package ID: 	knb-lter-mcr.8.35 (in the https://pasta.edirepository.org Catalog System)
# Data set title: MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Benthic Algae and Other Community Components, ongoing since 2005
# Data set creator:  Carpenter, Robert (Moorea Coral Reef LTER)
# Metadata Provider:  Moorea Coral Reef LTER
# Contact:    -  MCR LTER  - mcrlter@msi.ucsb.edu

# Load in algae data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/8/35/54d54c25616a48b9ec684118df9d6fca"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

bnt <-read.csv(infile1,header=T, 
  check.names=TRUE)

unlink(infile1)

#Remove Years with missing data
bnt_d = bnt %>% 
  filter(Habitat != 'Outer 17') %>%
  filter(Year != '2005') %>%
  filter(Year != '2006') %>%
  filter(Year != '2021') %>%
  filter(Year != '2022') %>%
  select(- Location) %>%
  mutate(Site = ifelse(Site == 'LTER 1', '1', 
                       ifelse(Site == 'LTER 2', '2', 
                              ifelse(Site == 'LTER 3', '3', 
                                     ifelse(Site == 'LTER 4', '4', 
                                            ifelse(Site == 'LTER 5', '5', '6')))))) %>%
  mutate(Habitat = ifelse(Habitat == 'Fringing', 'FR', 
                       ifelse(Habitat == 'Backreef', 'BA', 
                              ifelse(Habitat == 'Outer 10', 'FO', NA))))

#Ensure data classes match between algae and fish data
if (class(mcr_bnt$Site)!='factor') mcr_bnt$Site<- as.factor(mcr_bnt$Site)
if (class(mcr_bnt$Year)!='numeric') mcr_bnt$Year<- as.numeric(mcr_bnt$Year)

#
mcr_wide_cover = pivot_wider(bnt_d, 
                               id_cols = c(Year, Site, Habitat),
                               names_from = Taxonomy_Substrate_Functional_Group, 
                               values_from = Percent_Cover,
                               values_fn = sum,
                               values_fill = 0) 
mcr_long_cover = pivot_longer(mcr_wide_cover, cols = 'Algal Turf':'Colpomenia sinuosa', names_to = 'Taxonomy_Substrate_Functional_Group', values_to = 'Percent_Cover')


mcr_bnt = mcr_long_cover %>%
  group_by(Year, Site, Habitat, Taxonomy_Substrate_Functional_Group) %>%
  summarize(Percent_Cover = sum(Percent_Cover/50, na.rm = TRUE)) %>%
  ungroup()

#Classify the benthic cover into broader functional groups 
mcr_func <- mcr_bnt %>%
  mutate(Functional_Group = ifelse(Taxonomy_Substrate_Functional_Group == "Coral Rubble"| Taxonomy_Substrate_Functional_Group =="Crustose Corallines"| Taxonomy_Substrate_Functional_Group =="Sand"| Taxonomy_Substrate_Functional_Group =="Cyanophyta"| Taxonomy_Substrate_Functional_Group =="Sponge"| Taxonomy_Substrate_Functional_Group =="Soft Coral" | Taxonomy_Substrate_Functional_Group =="Coral", 'Non-algae', ifelse(Taxonomy_Substrate_Functional_Group== 'Algal Turf'| Taxonomy_Substrate_Functional_Group== 'Damselfish Turf'| Taxonomy_Substrate_Functional_Group == 'Bare Space', "Turf Algae", "Macroalgae")))

# Fill benthic cover with 0s at sites with no observations
mcr_wide_cover = pivot_wider(mcr_func, 
                               id_cols = c(Year, Site, Habitat),
                               names_from = Functional_Group, 
                               values_from = Percent_Cover,
                               values_fn = sum,
                               values_fill = 0) 


# Package ID: 	knb-lter-mcr.6.62 (in the https://pasta.edirepository.org Catalog System)
# Data set title: MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005
# Data set creator:  Brooks, Andrew (Moorea Coral Reef LTER)
# Metadata Provider:  Moorea Coral Reef LTER
# Contact:    -  MCR LTER  - mcrlter@msi.ucsb.edu


# Load in fish data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/6/62/ac2c7a859ce8595ec1339e8530b9ba50"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method='curl'))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


fsh <-read.csv(infile1,header=T, 
               check.names=TRUE)
unlink(infile1)

# Ensure data classes match between algae and fish datasets
if (class(fsh$Site)!="factor") fsh$Site<- as.factor(fsh$Site)
if (class(fsh$Year)!="numeric") fsh$Year<- as.numeric(fsh$Year)

# Get rid of unnecessary columns and years with missing data
mcr = fsh %>%
  filter(Taxonomy!= '') %>%
  filter(Year != '2006') %>%
  filter(Year != '2021') %>%
  filter(Year != '2020') %>%
  select(-Cloud_Cover, -Wind_Velocity, -Sea_State, -Swell, -Visibility, -Surge, -Diver, -Length_Anomaly, -Start, -End, -Comment)

# Fill herbivore biomass with 0s at sites with no observations
mcr_wide_biomass = pivot_wider(mcr, 
                             id_cols = c(Year, Site, Habitat, Transect ,Swath),
                             names_from = Taxonomy, 
                             values_from = Biomass,
                             values_fn = sum,
                             values_fill = 0) 

mcr_long_biomass = pivot_longer(mcr_wide_biomass, cols = 'Canthigaster solandri':'Unidentified Parrotfish 8', names_to = 'Taxonomy', values_to = 'Biomass')

#Filter for herbivores
fsh_id = mcr_long_biomass %>%
  mutate(Family = fsh$Family[match(mcr_long_biomass$Taxonomy,fsh$Taxonomy)]) %>%
  mutate(Coarse_Trophic = fsh$Coarse_Trophic[match(mcr_long_biomass$Taxonomy,fsh$Taxonomy)]) %>%
  mutate(Fine_Trophic = fsh$Fine_Trophic[match(mcr_long_biomass$Taxonomy,fsh$Taxonomy)]) %>%
  filter(Fine_Trophic == c('Cropper', 'Scraper', 'Browser', 'Brusher', 'Excavator', 'Herbivore/Detritivore', 'Concealed Cropper', 'Sediment Sucker'))
#Standardize biomass by area
mcr_fsh = fsh_id %>%
  mutate(Biomass_per_meter = Biomass/(Swath * 50)) %>%
  group_by(Year, Site, Habitat, Coarse_Trophic) %>%
  summarize(Average_Biomass = sum(Biomass_per_meter, na.rm = TRUE)) %>%
  ungroup() 

mcr_wide_herbivore = pivot_wider(mcr_fsh, 
                               id_cols = c(Year, Site, Habitat),
                               names_from = Coarse_Trophic, 
                               values_from = Average_Biomass,
                               values_fn = mean,
                               values_fill = 0) 

# combine the algae and fish data
mcr_prod_cons = left_join(mcr_fsh, mcr_wide_cover,
                          by = c('Year', 'Site', 'Habitat'))

mcr_prod_cons= mcr_prod_cons%>%
  mutate(Cyclone_Disturbance = case_when(Year < 2010 ~ 'pre',
                                         Year >= 2010 ~ 'post')) 
#Save file
#write.csv(mcr_prod_cons, file = "MCR_prod_cons.csv")
