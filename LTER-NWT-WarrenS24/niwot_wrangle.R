## --------------------------------------------------- ##
                  # Wrangle Niwot Data
## --------------------------------------------------- ##
# Written by: Nick Lyon, 

## ---------------------------------- ##
# Houskeeping ----
## ---------------------------------- ##

# Load any needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

## ---------------------------------- ##
# Download Data ----
## ---------------------------------- ##

# Download a local copy of the data using the R code from EDI
source(file = file.path("LTER-NWT-WarrenS24", "niwot_edi_download.R"))
## NOTE: takes a few moments to churn through this script

## ---------------------------------- ##
# Subplot Spp. Comp + Soil Data ----
## ---------------------------------- ##

# PURPOSE:
## Combine species composition at the subplot level with soil sensor data

# Read in the relevant files
comp_v1 <- read.csv(file = file.path("LTER-NWT-WarrenS24", "spp-comp-subplot.csv"))
soil_v1 <- read.csv(file = file.path("LTER-NWT-WarrenS24", "soil-sensor-data.csv"))

# Check structure of both files
dplyr::glimpse(comp_v1)
dplyr::glimpse(soil_v1)

# Check to see if site column is shared
supportR::diff_check(old = unique(comp_v1$local_site), new = unique(soil_v1$local_site))
## Some differences... Looks like site is abbreviated in the soil data

# Wrangle soil data as needed
soil_v2 <- soil_v1 %>%
  # Drop all "flag" columns (can always get them back later if desired)
  dplyr::select(-dplyr::starts_with("flag_")) %>%
  # Rename some columns more descriptively
  dplyr::rename(date_time = date,
                site_treat = local_site) %>%
  # Split up site and treatment to match with the species composition data
  tidyr::separate_wider_delim(cols = site_treat, delim = "_", 
                              names = c("site_abbrev", "treat_abbrev")) %>%
  # Get full site names from site abbreviations
  dplyr::mutate(local_site = dplyr::case_when(
    site_abbrev == "a" ~ "Audubon",
    site_abbrev == "ek" ~ "East_Knoll",
    site_abbrev == "l" ~ "Lefty",
    site_abbrev == "s" ~ "Soddie",
    site_abbrev == "t" ~ "Trough")) %>%
  # Expand treatment abbreviations in the same way
  dplyr::mutate(treatment = dplyr::case_when(
    treat_abbrev == "bs" ~ "Early",
    treat_abbrev == "con" ~ "Control")) %>%
  # Get date separated from time
  dplyr::mutate(date_collected = stringr::str_sub(string = date_time, start = 1, end = 10)) %>%
  # Average across hours within a given date
  dplyr::group_by(LTER_site, date_collected, local_site, treatment) %>%
  dplyr::summarize(soiltemp5_5cm_avg = mean(soiltemp5_5cm_avg, na.rm = TRUE),
                   ec5_5cm_avg = mean(ec5_5cm_avg, na.rm = TRUE),
                   soilmoisture5_5cm_avg = mean(soilmoisture5_5cm_avg, na.rm = TRUE),
                   soiltemp6_5cm_avg = mean(soiltemp6_5cm_avg, na.rm = TRUE),
                   ec6_5cm_avg = mean(ec6_5cm_avg, na.rm = TRUE),
                   soilmoisture6_5cm_avg = mean(soilmoisture6_5cm_avg, na.rm = TRUE),
                   soiltemp7_5cm_avg = mean(soiltemp7_5cm_avg, na.rm = TRUE),
                   ec7_5cm_avg = mean(ec7_5cm_avg, na.rm = TRUE),
                   soilmoisture7_5cm_avg = mean(soilmoisture7_5cm_avg, na.rm = TRUE),
                   soiltemp8_5cm_avg = mean(soiltemp8_5cm_avg, na.rm = TRUE),
                   ec8_5cm_avg = mean(ec8_5cm_avg, na.rm = TRUE),
                   soilmoisture8_5cm_avg = mean(soilmoisture8_5cm_avg, na.rm = TRUE),
                   batt_volt_min = mean(batt_volt_min, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  # Drop rows that are mostly missing information
  dplyr::filter(LTER_site != "NaN")
  
# Re-check structure
dplyr::glimpse(soil_v2)

# Confirm the two data files share the following information:
## LTER
supportR::diff_check(old = unique(comp_v1$LTER_site), new = unique(soil_v2$LTER_site))

## Site names
supportR::diff_check(old = unique(comp_v1$local_site), new = unique(soil_v2$local_site))

## Treatments
supportR::diff_check(old = unique(comp_v1$treatment), new = unique(soil_v2$treatment))

## Collection dates
supportR::diff_check(old = unique(comp_v1$date_collected), new = unique(soil_v2$date_collected))
### Lot of dates in one but not the other but that's probably fine

# Combine the two data files
combo <- dplyr::left_join(x = comp_v1, y = soil_v2, 
                          by = c("LTER_site", "local_site", "treatment", "date_collected"))

# Check structure of that
dplyr::glimpse(combo)

## ---------------------------------- ##
# EDI Data + Sconiers data
## ---------------------------------- ##

# Should follow similar pattern to above:
## 1) Identify columns in both data tables
## 2) Use `supportR::diff_check` to make sure they exactly match (make them if they don't)
## 3) Use `left_join` (or possibly `full_join`) to combine
## 4) Celebrate successful wrangling!




# End ----