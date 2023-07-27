################################################################################
##  Konza_timeSeries.R: Time series of resources, producers, and consumers.
##
##  Authors: Kimberly Komatsu
################################################################################

### TO DO ###
# include the spatial variability (plot all transects), instead of annual point
# add bison and grazed production data


library(grid)
library(gridExtra)
library(tidyverse)

setwd('G:\\Shared drives\\LTER-SPARC_Producers-Consumers-Disturbance')

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

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
  select(-growing_year) %>% 
  pivot_longer(cols=c('annual_rainfall', 'growing_season_rainfall'), names_to='scale', values_to='precip')

# figure #
precipFig <- ggplot(data=precipSummary, aes(x=RECYEAR, y=precip, color=scale)) +
  annotate("rect", xmin = 2010.5, xmax = 2012.5, ymin = 0, ymax = 1500,
           alpha = .2,fill = "blue") +
  annotate("rect", xmin = 1988.5, xmax = 1991.5, ymin = 0, ymax = 1500,
           alpha = .2,fill = "blue") +
  geom_point(size=2) +
  geom_line() +
  xlab('Growing Year') + ylab('Precipitation (mm)') +
  scale_color_manual(values=c('darkgrey', 'forestgreen'),
                     breaks=c("annual_rainfall", "growing_season_rainfall"),
                     labels=c("Annual", "Growing Season")) +
  theme(legend.position=c(0.4,0.05), legend.justification=c(0,0))


#### Productivity Data ####

# ungrazed #
plantBiomassUngrazed <- read.csv('Data/konza/PAB011.csv') %>% 
  filter(COMMENTS!='missing') %>% 
  mutate_at(c('LVGRASS', 'FORBS', 'CUYRDEAD', 'WOODY'), ~replace_na(.,0)) %>% 
  mutate(total_biomass=(LVGRASS+FORBS+CUYRDEAD+WOODY)) %>% 
  group_by(RECYEAR, WATERSHED, TRANSECT) %>% 
  summarise(total_biomass=10*mean(total_biomass)) %>% #mean within transect
  ungroup() %>% 
  group_by(RECYEAR, WATERSHED) %>%
  summarise(total_biomass=mean(total_biomass)) %>%
  ungroup()

# figure #
plantBiomassFig <- ggplot(data=plantBiomassUngrazed, aes(x=RECYEAR, y=total_biomass, color=WATERSHED)) +
  annotate("rect", xmin = 2010.5, xmax = 2012.5, ymin = 0, ymax = 800,
           alpha = .2,fill = "blue") +
  annotate("rect", xmin = 1988.5, xmax = 1991.5, ymin = 0, ymax = 800,
           alpha = .2,fill = "blue") +
  geom_point(size=2) +
  geom_line() +
  xlab('Growing Year') + ylab(expression(paste('Aboveground Biomass (g/',  ~ m ^ 2, ')'))) +
  scale_color_manual(values=c('darkorange', 'limegreen', 'purple'),
                     breaks=c("001d", "004b", "020b"),
                     labels=c("Annual Burn", "4 Yr", "20 Yr")) +
  theme(legend.position='none')


#### Grasshopper Data ####

grasshoppers <- read.csv('Data/konza/CGR022.csv') %>% 
  mutate(WATERSHED=ifelse(WATERSHED=='001D', '001d',
                   ifelse(WATERSHED=='002C', '002c',
                   ifelse(WATERSHED=='002D', '002d',
                   ifelse(WATERSHED=='004B', '004b',
                   ifelse(WATERSHED=='004F', '004f',
                   ifelse(WATERSHED=='020B', '020b',
                   ifelse(WATERSHED=='0SPB', '0spb',
                   ifelse(WATERSHED=='0SUB', '0sub',
                   ifelse(WATERSHED=='N01A', 'n01a',
                   ifelse(WATERSHED=='N01B', 'n01b',
                   ifelse(WATERSHED=='N04A', 'n04a',
                   ifelse(WATERSHED=='N04D', 'n04d',
                   ifelse(WATERSHED=='N20A', 'n20a',
                   ifelse(WATERSHED=='N20B', 'n20b', WATERSHED))))))))))))))) %>% 
  mutate(TOTAL=ifelse(TOTAL=='', 0, TOTAL)) %>% 
  mutate(TOTAL=as.numeric(TOTAL)) %>% 
  group_by(RECYEAR, RECMONTH, WATERSHED, REPSITE) %>% 
  summarise(grasshopper_abundance=sum(TOTAL)) %>% #average by sample site
  ungroup() %>% 
  filter(!is.na(grasshopper_abundance), !is.na(RECYEAR)) %>% 
  group_by(RECYEAR, WATERSHED, REPSITE) %>%
  summarise(grasshopper_abundance=max(grasshopper_abundance)) %>% #decision point: took max across multiple monthly samples, because the number of months sampled for each year was not consistent
  ungroup() %>% 
  group_by(RECYEAR, WATERSHED) %>%
  summarise(grasshopper_abundance=mean(grasshopper_abundance)) %>%
  ungroup()

# figure #
grasshopperFig <- ggplot(data=subset(grasshoppers, WATERSHED %in% c('001d', '004b', '020b')), aes(x=RECYEAR, y=grasshopper_abundance, color=WATERSHED)) +
  annotate("rect", xmin = 2010.5, xmax = 2012.5, ymin = 0, ymax = 1000,
           alpha = .2,fill = "blue") +
  annotate("rect", xmin = 1988.5, xmax = 1991.5, ymin = 0, ymax = 1000,
           alpha = .2,fill = "blue") +
  geom_point(size=2) +
  geom_line() +
  xlab('Growing Year') + ylab('Grasshopper Abundance') +
  scale_color_manual(values=c('darkorange', 'limegreen', 'purple'),
                     breaks=c("001d", "004b", "020b"),
                     labels=c("Annual Burn", "4 Yr", "20 Yr")) +
  # coord_cartesian(ylim=c(0,1200)) +
  theme(legend.position=c(0.6,0.95), legend.justification=c(1,1))


#### Bison Data ####

bison <- read.csv('Data/konza/CBH012.csv') %>% 
  rename(RECYEAR=RecYear) %>%
  filter(AnimalWeight!='.') %>% 
  mutate(AnimalWeight=as.numeric(AnimalWeight)) %>% 
  group_by(RECYEAR) %>% 
  summarise(bison_weight=mean(AnimalWeight, na.rm=T)) %>% 
  ungroup()
  
# figure #
bisonFig <- ggplot(data=subset(bison, RECYEAR>1995), aes(x=RECYEAR, y=bison_weight)) +
  annotate("rect", xmin = 2010.5, xmax = 2012.5, ymin = 650, ymax = 800,
           alpha = .2,fill = "blue") +
  annotate("rect", xmin = 1988.5, xmax = 1991.5, ymin = 650, ymax = 800,
           alpha = .2,fill = "blue") +
  geom_point(size=2) +
  geom_smooth(se=F, color='black') +
  coord_cartesian(xlim=c(1982,2022)) +
  xlab('Growing Year') + ylab('Bison Weight (lbs)')


#### Save Figure ####
ggsave('G:\\Shared drives\\LTER-SPARC_Producers-Consumers-Disturbance\\Figures\\0_RawTimeSeries\\KNZ_TimeSeries_20230726.png', 
       plot=grid.arrange(bisonFig, grasshopperFig, plantBiomassFig, precipFig, nrow=4),
       width=10, height=20, units='in', dpi=300, bg='white')





#### Ratio Time Series ####
prodConsRatio <- left_join(plantBiomassUngrazed, grasshoppers) %>% 
  mutate(PC_ratio=grasshopper_abundance/total_biomass)

prodConsRatioFig <- ggplot(data=prodConsRatio, aes(x=RECYEAR, y=PC_ratio, color=WATERSHED)) +
  annotate("rect", xmin = 2010.5, xmax = 2012.5, ymin = 0, ymax = 4,
           alpha = .2,fill = "blue") +
  annotate("rect", xmin = 1988.5, xmax = 1991.5, ymin = 0, ymax = 4,
           alpha = .2,fill = "blue") +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c('darkorange', 'limegreen', 'purple'),
                     breaks=c("001d", "004b", "020b"),
                     labels=c("Annual Burn", "4 Yr", "20 Yr")) +
  coord_cartesian(xlim=c(1982,2022)) +
  xlab('Growing Year') + ylab('Consumer:Producer') +
  theme(legend.position='none')

#### Save Figure ####
ggsave('G:\\Shared drives\\LTER-SPARC_Producers-Consumers-Disturbance\\Figures\\1_RatioTimeSeries\\KNZ_RatioTimeSeries_20230726.png', 
       plot=grid.arrange(prodConsRatioFig, grasshopperFig, plantBiomassFig, nrow=3),
       width=10, height=20, units='in', dpi=300, bg='white')



#### Phase-Space Diagram ####
phaseAnnual <- ggplot(data=subset(prodConsRatio, WATERSHED=='001d'),
                      aes(x=total_biomass, y=grasshopper_abundance, label=RECYEAR)) +
  geom_point(size=2) +
  geom_text(hjust=0, vjust=0) +
  geom_segment(color="darkorange", 
               aes(x=total_biomass,
                   y=grasshopper_abundance,
                   xend=c(tail(total_biomass, n=-1), NA), 
                   yend=c(tail(grasshopper_abundance, n=-1), NA)),
               arrow=arrow(length=unit(3,"mm"))) +
  xlab(expression(paste('Aboveground Biomass (g/',  ~ m ^ 2, ')'))) + 
  ylab('Grasshopper Abundance')

phase4Yr <- ggplot(data=subset(prodConsRatio, WATERSHED=='004b'),
       aes(x=total_biomass, y=grasshopper_abundance, label=RECYEAR)) +
  geom_point(size=2) +
  geom_text(hjust=0, vjust=0) +
  geom_segment(color="limegreen", 
               aes(x=total_biomass,
                   y=grasshopper_abundance,
                   xend=c(tail(total_biomass, n=-1), NA), 
                   yend=c(tail(grasshopper_abundance, n=-1), NA)),
                   arrow=arrow(length=unit(3,"mm"))) +
  xlab(expression(paste('Aboveground Biomass (g/',  ~ m ^ 2, ')'))) + 
  ylab('Grasshopper Abundance')

phase20Yr <- ggplot(data=subset(prodConsRatio, WATERSHED=='020b'),
                      aes(x=total_biomass, y=grasshopper_abundance, label=RECYEAR)) +
  geom_point(size=2) +
  geom_text(hjust=0, vjust=0) +
  geom_segment(color="purple", 
               aes(x=total_biomass,
                   y=grasshopper_abundance,
                   xend=c(tail(total_biomass, n=-1), NA), 
                   yend=c(tail(grasshopper_abundance, n=-1), NA)),
               arrow=arrow(length=unit(3,"mm"))) +
  xlab(expression(paste('Aboveground Biomass (g/',  ~ m ^ 2, ')'))) + 
  ylab('Grasshopper Abundance')

#### Save Figure ####
ggsave('G:\\Shared drives\\LTER-SPARC_Producers-Consumers-Disturbance\\Figures\\1b_PhaseSpaceDiagrams\\KNZ_PhaseSpaceDiagram_20230726.png', 
       plot=grid.arrange(phaseAnnual, phase4Yr, phase20Yr, ncol=3),
       width=30, height=10, units='in', dpi=300, bg='white')
