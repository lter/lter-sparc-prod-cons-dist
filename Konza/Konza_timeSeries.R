################################################################################
##  Konza_timeSeries.R: Time series of resources, producers, and consumers.
##
##  Authors: Kimberly Komatsu
################################################################################

library(tidyverse)

setwd('G:\\Shared drives\\LTER-SPARC_Producers-Consumers-Disturbance')

#### Precip Data ####
precipMonthly <- read.csv('Data/konza/AWE012.csv') %>% 
  filter(!(DPPT %in% c('.', ''))) %>% 
  mutate(DPPT=as.numeric(DPPT)) %>% 
  group_by(RECYEAR, RECMONTH) %>% 
  summarise(monthy_rainfall=sum(DPPT)) %>% 
  ungroup()

precipAnnual <- precipMonthly %>% 
  mutate(growing_year=ifelse(RECMONTH>9, (RECYEAR+1), RECYEAR)) %>% 
  group_by(growing_year) %>% 
  summarise(annual_rainfall=sum(monthy_rainfall)) %>% 
  ungroup() %>% 
  filter(growing_year<2023)

precipGrowingSeason <- precipMonthly %>% 
  filter(RECMONTH %in% c(5, 6, 7, 8, 9)) %>% 
  group_by(RECYEAR) %>% 
  summarise(growing_season_rainfall=sum(monthy_rainfall)) %>% 
  ungroup()

precipSummary <- cbind(precipAnnual, precipGrowingSeason) %>% 
  select(-growing_year)


#### Productivity Data ####

# ungrazed #
plantBiomassUngrazed <- read.csv('Data/konza/PAB011.csv')


















