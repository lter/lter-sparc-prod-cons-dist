# ====================================================================================
# SPARC: LTER Producers-Consumers-Disturbance
# Code for SBC LTER
# ====================================================================================

# Max Castorani
# with some code adapted from Mae Rennick, Adrian Stier (Rennick et al. 2022)
# Revised 2023-11-08

# ====================================================================================
# SOURCE SCRIPTS AND DATA
# ====================================================================================
# Clear environment
rm(list = ls())

# ------------------------------------------------------------------------------------
# Set working environment
library(here)
setwd("/Users/castorani/Sync/Documents/Research/Active projects/SPARC working group (Marrec, Wilson)/")
here::here("/Users/castorani/Sync/Documents/Research/Active projects/SPARC working group (Marrec, Wilson)/")

# ------------------------------------------------------------------------------------
# Turn off scientific notation
options(scipen=999)

# ------------------------------------------------------------------------------------
# Load libraries and custom functions
library(tidyverse)
library(ggrepel)

# ------------------------------------------------------------------------------------
# Load and tidy data (section-level)
source("Code/import.and.tidy.data.R")

# ------------------------------------------------------------------------------------
# Drop year 2000 (first year of data collection)
dat.long <- dat.long[dat.long$year > 2000, ]
dat.wide <- dat.wide[dat.wide$year > 2000, ] 

# ====================================================================================
# FILTER DATA
# ====================================================================================

# Rearrange data to wide and select only giant kelp and urchins
dat.wide <- dat.long %>%
  dplyr::rename(species = 'trophic.group') %>%
  dplyr::filter(species == 'giant.kelp.dry.gm2' | species == 'urchin.dry.gm2') %>%
  tidyr::spread(key = species, value = biomass, fill = NA) 

# ====================================================================================
# DATA FOR INFORMED STATIC DIAGRAM TABLE
# ====================================================================================

# Calculate mean kelp and urchin biomass before the decline and after (3 year periods each)
mean(dat.wide$giant.kelp.dry.gm2[dat.wide$year >= 2012 & dat.wide$year <= 2014])
mean(dat.wide$urchin.dry.gm2[dat.wide$year >= 2012 & dat.wide$year <= 2014])

mean(dat.wide$giant.kelp.dry.gm2[dat.wide$year >= 2015 & dat.wide$year <= 2017])
mean(dat.wide$urchin.dry.gm2[dat.wide$year >= 2015 & dat.wide$year <= 2017])

# ------------------------------------------------------------------------------------
# Convert to fluxes: Kelp NPP


# ------------------------------------------------------------------------------------
# Convert to fluxes: Urchin herbivory

# Sum urchin abundance by species to demonstrate that it is just two species that are overwhelmingly important and the other two species have negligible impact on grazing rates
dat.urchins.by.species %>%
  dplyr::group_by(taxon) %>%
  dplyr::summarise(dry.gm2 = mean(dry.gm2))



# ====================================================================================
# CALCULATE AND PLOT Z-SCORES AND BIOMASS RATIOS
# ====================================================================================

# Calculate ratio
dat.wide$ratio = dat.wide$urchin.dry.gm2 / dat.wide$giant.kelp.dry.gm2

# ------------------------------------------------------------------------------------
# Convert to z-score (within sites and across years)
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

dat.z.score <-  dat.wide %>%
  dplyr::select(id:plot, giant.kelp.dry.gm2, urchin.dry.gm2) %>%
  droplevels() %>%
  group_by(site) %>%
  dplyr::mutate(urchin.z = scale2(urchin.dry.gm2),
                kelp.z = scale2(giant.kelp.dry.gm2)) %>%
  dplyr::select(-giant.kelp.dry.gm2, -urchin.dry.gm2) %>%
  gather(., key = 'species', value = 'z.score',
         -id, -year, -site, -transect, -plot)

# ------------------------------------------------------------------------------------
# Plot by site and year
dat.z.score.by.site.year <- dat.z.score %>%
  dplyr::group_by(site, year, species) %>%
  dplyr::summarise(z.score = mean(z.score, na.rm = T)) %>%
  ungroup() 

dat.z.score.by.site.year$species[dat.z.score.by.site.year$species == "urchin.z" ] <- "Urchins"
dat.z.score.by.site.year$species[dat.z.score.by.site.year$species == "kelp.z" ] <- "Giant kelp"

# ------------------------------------------------------------------------------------
# Plot by  year
dat.z.score.by.year <- dat.z.score %>%
  dplyr::group_by(year, species) %>%
  dplyr::summarise(z.score = mean(z.score, na.rm = T)) %>%
  ungroup() 

dat.z.score.by.year$species[dat.z.score.by.year$species == "urchin.z" ] <- "Urchins"
dat.z.score.by.year$species[dat.z.score.by.year$species == "kelp.z" ] <- "Giant kelp"

ggplot(data = dat.z.score.by.year, aes(x = year, y = z.score, color = species)) +
  geom_rect(fill = 'grey', color = NA, alpha = 0.1, xmin = 2014.5, xmax = 2016.5, 
            ymin = min(dat.z.score.by.site.year$z.score), ymax = max(dat.z.score.by.site.year$z.score)) +
  geom_line(linewidth = 1)+
  geom_point(size = 1.2)+
  scale_color_manual(values = c("#006d2c", "#810f7c"), name = NULL) +
  labs(x = "", y = "Normalized biomass")+
  theme(legend.position = 'top') +
  theme(panel.grid = element_blank()) +
  theme(legend.key = element_rect(fill = 'white'),
        text = element_text(size = 16),
        plot.title = element_text(size = 14),
        strip.background = element_blank(),
        strip.placement = "inside",
        strip.text = element_text(size=14, color = "black", hjust = 0),
        panel.spacing = unit(0.2, "lines"), 
        panel.background=element_rect(fill="white"),
        panel.border=element_rect(colour="black", size=1, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(margin=margin(0.1,0.1,0.1,0.1, "cm"), color = "black", size = 12),
        axis.text.y = element_text(margin=margin(0.1,0.1,0.1,0.1, "cm"), color = "black", size = 12),
        axis.line.x = element_line(color="black", size = 0),
        axis.line.y = element_line(color="black", size = 0),
        axis.ticks.y = element_line(size = 0.5, color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.x = element_line(size = 0.5, color = "black"),
        axis.ticks.length.x = unit(0.15, "cm"),
        aspect.ratio = 1/2) 
  
ggsave(filename = "Plots/SBC_TimeSeries.pdf", height = 3, width = 6)
ggsave(filename = "Plots/SBC_TimeSeries.jpg", height = 3, width = 6)


# ====================================================================================
# PLOT PHASE DIAGRAMS
# ====================================================================================

# ------------------------------------------------------------------------------------
# Z-score
dat.z.score.by.year.wide <- tidyr::spread(dat.z.score.by.year, key = species, value = z.score, fill = NA)

# ------------------------------------------------------------------------------------
# Categorize years
dat.z.score.by.year.wide$dummy <- "a"
dat.z.score.by.year.wide$dummy[dat.z.score.by.year.wide$year >= 2015 & dat.z.score.by.year.wide$year <= 2017] <- "b"

# ------------------------------------------------------------------------------------
# Phase diagram of z-scores by year only
ggplot(data = dat.z.score.by.year.wide, aes(x = `Giant kelp`, y = Urchins)) +
  geom_path(color = 'black') +
  geom_point(size = 2, aes(color = dummy)) +
  geom_text_repel(aes(label = year),
                  size = 2.5,
                   box.padding   = 0.3, 
                   point.padding = 0.5,
                   segment.color = NA) +
  # geom_text(aes(label = year),
  #           position = position_dodge(width = 1),
  #           vjust = 2, hjust = 0, size = 2) +
  theme_bw()+
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        plot.title = element_text(size = 14),
        strip.background = element_blank(),
        strip.placement = "inside",
        strip.text = element_text(size=14, color = "black", hjust = 0),
        panel.spacing = unit(0.2, "lines"), 
        panel.background=element_rect(fill="white"),
        panel.border=element_rect(colour="black", size=1, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(margin=margin(0.1,0.1,0.1,0.1, "cm"), color = "black", size = 12),
        axis.text.y = element_text(margin=margin(0.1,0.1,0.1,0.1, "cm"), color = "black", size = 12),
        axis.line.x = element_line(color="black", size = 0),
        axis.line.y = element_line(color="black", size = 0),
        axis.ticks.y = element_line(size = 0.5, color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.x = element_line(size = 0.5, color = "black"),
        axis.ticks.length.x = unit(0.15, "cm"),
        aspect.ratio = 1)  +
  scale_color_manual(values = c("blue", "red")) +
  guides(color = FALSE) +
  xlab("Normalized giant kelp biomass") +
  ylab("Normalized urchin biomass") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1))
ggsave(filename = "Plots/SBC_PhaseSpaceDiagram_Z.pdf", height = 4, width = 4)
ggsave(filename = "Plots/SBC_PhaseSpaceDiagram_Z.jpg", height = 4, width = 4)

