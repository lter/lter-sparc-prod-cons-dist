# ====================================================================================
# IMPORT AND TIDY DATA FROM SBC-LTER LONG TERM KELP FOREST MONITORING
# ====================================================================================

# Import data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/13/24d18d9ebe4f6e8b94e222840096963c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "VIS",     
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "DENSITY",     
                 "WM_GM2",     
                 "DRY_GM2",     
                 "SFDM",     
                 "AFDM",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_PHYLUM",     
                 "TAXON_CLASS",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "MOBILITY",     
                 "GROWTH_MORPH",     
                 "COARSE_GROUPING"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$VIS)=="factor") dt1$VIS <-as.numeric(levels(dt1$VIS))[as.integer(dt1$VIS) ]               
if (class(dt1$VIS)=="character") dt1$VIS <-as.numeric(dt1$VIS)
if (class(dt1$SP_CODE)!="factor") dt1$SP_CODE<- as.factor(dt1$SP_CODE)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)
if (class(dt1$DENSITY)=="factor") dt1$DENSITY <-as.numeric(levels(dt1$DENSITY))[as.integer(dt1$DENSITY) ]               
if (class(dt1$DENSITY)=="character") dt1$DENSITY <-as.numeric(dt1$DENSITY)
if (class(dt1$WM_GM2)=="factor") dt1$WM_GM2 <-as.numeric(levels(dt1$WM_GM2))[as.integer(dt1$WM_GM2) ]               
if (class(dt1$WM_GM2)=="character") dt1$WM_GM2 <-as.numeric(dt1$WM_GM2)
if (class(dt1$DRY_GM2)=="factor") dt1$DRY_GM2 <-as.numeric(levels(dt1$DRY_GM2))[as.integer(dt1$DRY_GM2) ]               
if (class(dt1$DRY_GM2)=="character") dt1$DRY_GM2 <-as.numeric(dt1$DRY_GM2)
if (class(dt1$SFDM)=="factor") dt1$SFDM <-as.numeric(levels(dt1$SFDM))[as.integer(dt1$SFDM) ]               
if (class(dt1$SFDM)=="character") dt1$SFDM <-as.numeric(dt1$SFDM)
if (class(dt1$AFDM)=="factor") dt1$AFDM <-as.numeric(levels(dt1$AFDM))[as.integer(dt1$AFDM) ]               
if (class(dt1$AFDM)=="character") dt1$AFDM <-as.numeric(dt1$AFDM)
if (class(dt1$SCIENTIFIC_NAME)!="factor") dt1$SCIENTIFIC_NAME<- as.factor(dt1$SCIENTIFIC_NAME)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$TAXON_KINGDOM)!="factor") dt1$TAXON_KINGDOM<- as.factor(dt1$TAXON_KINGDOM)
if (class(dt1$TAXON_PHYLUM)!="factor") dt1$TAXON_PHYLUM<- as.factor(dt1$TAXON_PHYLUM)
if (class(dt1$TAXON_CLASS)!="factor") dt1$TAXON_CLASS<- as.factor(dt1$TAXON_CLASS)
if (class(dt1$TAXON_ORDER)!="factor") dt1$TAXON_ORDER<- as.factor(dt1$TAXON_ORDER)
if (class(dt1$TAXON_FAMILY)!="factor") dt1$TAXON_FAMILY<- as.factor(dt1$TAXON_FAMILY)
if (class(dt1$TAXON_GENUS)!="factor") dt1$TAXON_GENUS<- as.factor(dt1$TAXON_GENUS)
if (class(dt1$GROUP)!="factor") dt1$GROUP<- as.factor(dt1$GROUP)
if (class(dt1$MOBILITY)!="factor") dt1$MOBILITY<- as.factor(dt1$MOBILITY)
if (class(dt1$GROWTH_MORPH)!="factor") dt1$GROWTH_MORPH<- as.factor(dt1$GROWTH_MORPH)
if (class(dt1$COARSE_GROUPING)!="factor") dt1$COARSE_GROUPING<- as.factor(dt1$COARSE_GROUPING)

# Convert Missing Values to NA for non-dates

dt1$VIS <- ifelse((trimws(as.character(dt1$VIS))==trimws("-99999")),NA,dt1$VIS)               
suppressWarnings(dt1$VIS <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$VIS))==as.character(as.numeric("-99999"))),NA,dt1$VIS))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))
dt1$DENSITY <- ifelse((trimws(as.character(dt1$DENSITY))==trimws("-99999")),NA,dt1$DENSITY)               
suppressWarnings(dt1$DENSITY <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DENSITY))==as.character(as.numeric("-99999"))),NA,dt1$DENSITY))
dt1$WM_GM2 <- ifelse((trimws(as.character(dt1$WM_GM2))==trimws("-99999")),NA,dt1$WM_GM2)               
suppressWarnings(dt1$WM_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$WM_GM2))==as.character(as.numeric("-99999"))),NA,dt1$WM_GM2))
dt1$DRY_GM2 <- ifelse((trimws(as.character(dt1$DRY_GM2))==trimws("-99999")),NA,dt1$DRY_GM2)               
suppressWarnings(dt1$DRY_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DRY_GM2))==as.character(as.numeric("-99999"))),NA,dt1$DRY_GM2))
dt1$SFDM <- ifelse((trimws(as.character(dt1$SFDM))==trimws("-99999")),NA,dt1$SFDM)               
suppressWarnings(dt1$SFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SFDM))==as.character(as.numeric("-99999"))),NA,dt1$SFDM))
dt1$AFDM <- ifelse((trimws(as.character(dt1$AFDM))==trimws("-99999")),NA,dt1$AFDM)               
suppressWarnings(dt1$AFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$AFDM))==as.character(as.numeric("-99999"))),NA,dt1$AFDM))
dt1$SCIENTIFIC_NAME <- as.factor(ifelse((trimws(as.character(dt1$SCIENTIFIC_NAME))==trimws("-99999")),NA,as.character(dt1$SCIENTIFIC_NAME)))
dt1$TAXON_PHYLUM <- as.factor(ifelse((trimws(as.character(dt1$TAXON_PHYLUM))==trimws("-99999")),NA,as.character(dt1$TAXON_PHYLUM)))
dt1$TAXON_CLASS <- as.factor(ifelse((trimws(as.character(dt1$TAXON_CLASS))==trimws("-99999")),NA,as.character(dt1$TAXON_CLASS)))
dt1$TAXON_ORDER <- as.factor(ifelse((trimws(as.character(dt1$TAXON_ORDER))==trimws("-99999")),NA,as.character(dt1$TAXON_ORDER)))
dt1$TAXON_FAMILY <- as.factor(ifelse((trimws(as.character(dt1$TAXON_FAMILY))==trimws("-99999")),NA,as.character(dt1$TAXON_FAMILY)))

raw.dat <- dt1
rm(dt1)
  
# ------------------------------------------------------------------------------------
# Load substrate data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/138/3/82d1b4ba2b2c1b5438ae1279e19bf68b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "QUAD",     
                 "SIDE",     
                 "SUBSTRATE_TYPE",     
                 "COMMON_NAME",     
                 "PERCENT_COVER"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$QUAD)!="factor") dt1$QUAD<- as.factor(dt1$QUAD)
if (class(dt1$SIDE)!="factor") dt1$SIDE<- as.factor(dt1$SIDE)
if (class(dt1$SUBSTRATE_TYPE)!="factor") dt1$SUBSTRATE_TYPE<- as.factor(dt1$SUBSTRATE_TYPE)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)

# Convert Missing Values to NA for non-dates

dt1$SITE <- as.factor(ifelse((trimws(as.character(dt1$SITE))==trimws("-99999")),NA,as.character(dt1$SITE)))
dt1$TRANSECT <- as.factor(ifelse((trimws(as.character(dt1$TRANSECT))==trimws("-99999")),NA,as.character(dt1$TRANSECT)))
dt1$QUAD <- as.factor(ifelse((trimws(as.character(dt1$QUAD))==trimws("-99999")),NA,as.character(dt1$QUAD)))
dt1$SIDE <- as.factor(ifelse((trimws(as.character(dt1$SIDE))==trimws("-99999")),NA,as.character(dt1$SIDE)))
dt1$SUBSTRATE_TYPE <- as.factor(ifelse((trimws(as.character(dt1$SUBSTRATE_TYPE))==trimws("-99999")),NA,as.character(dt1$SUBSTRATE_TYPE)))
dt1$COMMON_NAME <- as.factor(ifelse((trimws(as.character(dt1$COMMON_NAME))==trimws("-99999")),NA,as.character(dt1$COMMON_NAME)))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))

substrate.monitoring.long <- dt1
rm(dt1)


# ====================================================================================
# DATA FORMATTING
# ====================================================================================
# Remove year 2000
#raw.dat <- raw.dat[raw.dat$YEAR > 2000, ]
#substrate.monitoring.long <- substrate.monitoring.long[substrate.monitoring.long$YEAR > 2000, ]

# ------------------------------------------------------------------------------------
# Replace underscores with dots for convenience. Also convert to lowercase
colnames(raw.dat) <- tolower(gsub("_", ".", colnames(raw.dat)))  
colnames(substrate.monitoring.long) <- tolower(gsub("_", ".", colnames(substrate.monitoring.long)))  

# ------------------------------------------------------------------------------------
# Check that the data complies
raw.dat2 <- raw.dat %>%
  dplyr::select(year, site, transect, scientific.name, sp.code, group, coarse.grouping, dry.gm2, afdm) 

# ------------------------------------------------------------------------------------
# Replace "-99999" values with NA
raw.dat[raw.dat == -99999] <- NA
substrate.monitoring.long[substrate.monitoring.long == -99999] <- NA

# ------------------------------------------------------------------------------------
# Fix substrate data
substrate.monitoring.long$substrate.type <- dplyr::recode(substrate.monitoring.long$substrate.type,
                                                          'B' = 'bedrock',
                                                          'BL' = 'boulder.large', 
                                                          'BM' = 'boulder.medium', 
                                                          'BS' = 'boulder.small',
                                                          'C' = 'cobble', 
                                                          'S' = 'sand', 
                                                          'SH' = 'shell.debris', 
                                                          'SS' = 'shallow.sand') 

substrate.monitoring.wide <- substrate.monitoring.long %>%
  dplyr::select(-common.name) %>%
  spread(key = substrate.type, value = percent.cover, fill = 0) %>%
  mutate(hard.substrate = bedrock + boulder.large + boulder.medium +  boulder.small + cobble,
         soft.substrate = sand + shallow.sand + shell.debris) 

# ------------------------------------------------------------------------------------
# Next, average at each time point (because these are percentages)
substrate.monitoring <- substrate.monitoring.wide %>%
  dplyr::group_by(year, month, date, site, transect) %>%
  dplyr::summarise(substrate.avg.prop.hard = mean(hard.substrate, na.rm = TRUE)/100,
                   substrate.avg.prop.soft = mean(soft.substrate, na.rm = TRUE)/100) %>%
  ungroup()

rm(substrate.monitoring.long, substrate.monitoring.wide)

# ------------------------------------------------------------------------------------
# Remove CYOS_R, which was added part way through
dat <- raw.dat2 %>%
  dplyr::filter(sp.code != "CYOS_R") %>%
  droplevels()

# ------------------------------------------------------------------------------------
# Next, group by scientific name and sum biomass at each time point
dat2 <- dat %>%
  dplyr::group_by(year, site, transect, sp.code, scientific.name, group, coarse.grouping) %>%
  dplyr::summarise(dry.gm2 = sum(dry.gm2, na.rm = TRUE),
                   afdm = sum(afdm, na.rm = TRUE)) %>%
  dplyr::rename(taxon = scientific.name) %>%
  ungroup()

# ------------------------------------------------------------------------------------
dat3 <- dat2 %>%
  dplyr::mutate(plot = paste(site, transect, sep="_")) %>%
  dplyr::mutate(site = factor(plyr::revalue(site, c("ABUR" = "Arroyo Burro",
                                                    "AHND" = "Arroyo Hondo",
                                                    "AQUE" = "Arroyo Quemado",
                                                    "BULL" = "Bulito",
                                                    "CARP" = "Carpinteria",
                                                    "GOLB" = "Goleta Beach",
                                                    "IVEE" = "Isla Vista",
                                                    "MOHK" = "Mohawk",
                                                    "NAPL" = "Naples",
                                                    "SCDI" = "Santa Cruz Island, Diablo",
                                                    "SCTW" = "Santa Cruz Island, Twin Harbor West")),
                              levels = c("Arroyo Burro",
                                         "Arroyo Hondo",
                                         "Arroyo Quemado",
                                         "Bulito",
                                         "Carpinteria",
                                         "Goleta Beach",
                                         "Isla Vista",
                                         "Mohawk",
                                         "Naples",
                                         "Santa Cruz Island, Diablo",
                                         "Santa Cruz Island, Twin Harbor West"))) %>%
  dplyr::mutate(id = paste(year, plot, sep = ".")) %>%
  dplyr::select(id, year, site, transect, plot, sp.code, taxon, group, coarse.grouping, dry.gm2, afdm) %>%
  droplevels()

dat <- dat3
rm(dat2, dat3)

# ------------------------------------------------------------------------------------
substrate.monitoring <- substrate.monitoring %>%
  dplyr::mutate(plot = paste(site, transect, sep="_")) %>%
  dplyr::mutate(site = factor(plyr::revalue(site, c("ABUR" = "Arroyo Burro",
                                                    "AHND" = "Arroyo Hondo",
                                                    "AQUE" = "Arroyo Quemado",
                                                    "BULL" = "Bulito",
                                                    "CARP" = "Carpinteria",
                                                    "GOLB" = "Goleta Beach",
                                                    "IVEE" = "Isla Vista",
                                                    "MOHK" = "Mohawk",
                                                    "NAPL" = "Naples",
                                                    "SCDI" = "Santa Cruz Island, Diablo",
                                                    "SCTW" = "Santa Cruz Island, Twin Harbor West")),
                              levels = c("Arroyo Burro",
                                         "Arroyo Hondo",
                                         "Arroyo Quemado",
                                         "Bulito",
                                         "Carpinteria",
                                         "Goleta Beach",
                                         "Isla Vista",
                                         "Mohawk",
                                         "Naples",
                                         "Santa Cruz Island, Diablo",
                                         "Santa Cruz Island, Twin Harbor West"))) %>%
  dplyr::mutate(id = paste(year, plot, sep = ".")) %>%
  dplyr::select(id, year, site, transect, plot, substrate.avg.prop.hard, substrate.avg.prop.soft) %>%
  droplevels()


# ====================================================================================
# DATA SUBSETTING
# ====================================================================================
# Retain only understory algae, giant kelp, and herbivores
urchins <- c('CECO', 'LA', 'SFL', 'SPL')
grazers <- c('APCA','APVA','CECO','CYSP','HACO','HACR','HAKA','HARU','LA','LIGL','LIGS','MECR','NONO','PUPR','SFL','SFS','SPL','SPS','TESP')

dat.grazers <- dat %>%
  dplyr::filter(sp.code %in% grazers)

dat.urchins <- dat %>%
  dplyr::filter(sp.code %in% urchins)

dat.producers <- dat %>%
  dplyr::filter(group == 'ALGAE')

dat.understory <- dat %>%
  dplyr::filter(coarse.grouping == "UNDERSTORY ALGAE")

dat.giant.kelp <- dat %>%
  dplyr::filter(sp.code == 'MAPY')

dat.urchins.by.species <- dat.urchins

# ====================================================================================
# SUM GROUPS FOR EACH TRANSECT-YEAR COMBINATION
# ====================================================================================
dat.grazers <- dat.grazers %>%
  dplyr::group_by(id, year, site, transect, plot) %>%
  dplyr::summarise(grazer.dry.gm2 = sum(dry.gm2, na.rm = TRUE),
                   grazer.afdm = sum(afdm, na.rm = TRUE)) %>%
  dplyr::ungroup()

dat.urchins <- dat.urchins %>%
  dplyr::group_by(id, year, site, transect, plot) %>%
  dplyr::summarise(urchin.dry.gm2 = sum(dry.gm2, na.rm = TRUE),
                   urchin.afdm = sum(afdm, na.rm = TRUE)) %>%
  dplyr::ungroup()

dat.producers <- dat.producers %>%
  dplyr::group_by(id, year, site, transect, plot) %>%
  dplyr::summarise(producer.dry.gm2 = sum(dry.gm2, na.rm = TRUE)) %>%
  dplyr::ungroup()

dat.understory <- dat.understory %>%
  dplyr::group_by(id, year, site, transect, plot) %>%
  dplyr::summarise(understory.dry.gm2 = sum(dry.gm2, na.rm = TRUE)) %>%
  dplyr::ungroup()

dat.giant.kelp <- dat.giant.kelp %>%
  dplyr::group_by(id, year, site, transect, plot) %>%
  dplyr::summarise(giant.kelp.dry.gm2 = sum(dry.gm2, na.rm = TRUE)) %>%
  dplyr::ungroup()

# Combine
dat <- dplyr::left_join(dat.producers, dat.giant.kelp,
                        by = c('id', 'year', 'site', 'transect', 'plot')) %>%
  left_join(dat.understory,
            by = c('id', 'year', 'site', 'transect', 'plot')) %>%
  left_join(dat.grazers,
            by = c('id', 'year', 'site', 'transect', 'plot')) %>%
  left_join(dat.urchins,
            by = c('id', 'year', 'site', 'transect', 'plot')) 

# # ====================================================================================
# # REMOVE FIRST YEAR OF DATA DUE TO LOW CONFIDENCE
# # ====================================================================================

# dat <- dat[dat$year > 2000, ]

# # ====================================================================================
# # MAKE LONG VERSION
# # ====================================================================================

dat.wide <- dat

dat.long <- dat.wide %>%
  gather(key = "trophic.group", value = "biomass",
               -id, -year, -site, -transect, -plot) 


# # ====================================================================================
# # CLEAN UP
# # ====================================================================================
rm(dat, dat.giant.kelp, dat.grazers, dat.urchins, dat.producers, dat.understory, raw.dat, raw.dat2)
rm(grazers, urchins, infile1, inUrl1)


# 