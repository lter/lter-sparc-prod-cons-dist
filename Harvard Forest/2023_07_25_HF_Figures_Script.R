#Figures code for ESA 2023

#load libraries:
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(mgcv)
library(pROC)
library(ggforce)
library(hrbrthemes)
library(viridis)
library(gghighlight)
library(tidygam)
library(lattice)
library(tactile)


### MAKING SAMPLE TIME SERIES PLOTS:

# loading Landsat-based remote sensing data for forest condition scores (cs) and Tasseled Cap Greenness Index (tcg):
# NOTE FOR NICK AND ANGEL: These data are from a remote sensing product on Google Earth Engine,
# The columns include geo data (lat, lon) and index with the rest being monthly condition scores
# or TCG values for growing season months. The first part of this code loads the data (all is on github
# if it needs to be changed from the filepath on my local machine) and grabs the time series then 
# renames the columns so that their names can be used as dates.

# files for condition score (cs) and tasseled cap greenness (tcg)
cs_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_score_mean.csv"
tcg_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_tcg_mean.csv"
# load csvs:
cs <- read.csv(cs_file)
tcg <- read.csv(tcg_file)

# Harvard Forest data - Landsat time series at some of the sites:
# NOTE FOR NICK AND ANGEL: This is for choosing plotting either cs time series or tcg time series
# When using single site you can select that here:
#site <- 12 # 12 sites in data collection, pick 1-12
obs <- tcg  # data observations - tcg or cs file from above - tcg for TCG, cs for condition scores

#get just the tcg values from data frame:
time_series<-obs[,c(grep("^X",colnames(obs)))]

#remove unnecessary parts of colnames to just get dates:
colnames(time_series) <- str_replace_all(names(time_series), c("X"="", 
                                                               "_score_mean"="", 
                                                               "_tcg_mean"="",
                                                               "\\."="-"))

### VERSION FOR PLOTTING SINGLE SITE LINE ON PLOT: ------
#y <- as.numeric(time_series[site,31:80]) #currently Apr 2011-Aug 2020
#y <- recov <- time_series[site,c(first(grep("^2017", colnames(time_series))):ncol(time_series))]
#recov <- time_series[site,c(first(grep("^2017-06", colnames(time_series))):ncol(time_series))]

# ### Adding dummy data to time series of recovery (for months with no observations)
# # getting all months from beginning to end of recovery:
# allmonths <- as.character(
#   seq.Date(from = as.Date(first(names(recov))),
#   to = as.Date(last(names(recov))),
#   by="month"))
# #selecting months with no observations:
# no_obs_mos <- symdiff(names(recov), allmonths)
# #make dummy dataframe with correct dimensions:
# no_obs <- data.frame(matrix(data = NA, nrow = nrow(recov), ncol = length(no_obs_mos)))
# #add the column names to sort by date during cbind:
# colnames(no_obs) <- c(no_obs_mos)
# #cbind for time series:
# full_ts <- cbind(recov, no_obs)[order(c(names(recov),names(no_obs)))]
# 
# #x <- 1:length(y)
# # TO DO: NEED TO ADD NON-DATA MONTHS TO TIME SERIES 
# 
# sample_site <- data.frame(
#   month = colnames(full_ts),
#   value = as.numeric(full_ts/1000) # corrects for scaling in data from GEE product
# )
# colnames(sample_site) <- c("x","value")
# 
# ##GGPLOT CODE
# ##turn this stuff on and change name to save:
# #fname = "2023_07_20_Slow_Recov_sample_plot_166.tiff"
# #tiff(fname, units = "in", width=12, height=3, res=300)
# ggplot(data = sample_site, mapping = aes(x = as.Date(x), y = value)) +
#   geom_line(color="forestgreen") +
#   geom_point(color="forestgreen") + 
#   geom_line(data = filter(sample_site, is.na(value)==FALSE), 
#             linetype = "dashed", color="forestgreen", size=0.3) +
#   #ylim(c(-3.2, 2)) +
#   labs(title = "Post-Defoliation Recovery",
#        y="Forest Condition Score", #or TCG - remember to change when making plots
#        x="Time (Months)") +
#   #scale_x_continuous(breaks = seq(min(x), max(x), by = 5)) +
#   theme_bw() + theme(panel.border = element_blank(), 
#                      panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), 
#                      axis.line = element_line(colour = "black"))
#   # theme(panel.grid.major = element_blank(), 
#   #       panel.grid.minor = element_blank())
# #dev.off()


### VERSION FOR PLOTTING ALL SITES - adding single line for each site: ------------
# make full time series with missing dummy variables -

# start with grabbing section of time series that you want to plot:
# FULL TIME SERIES recov object is just the time_series object (see below)
# RECOVERY WINDOW: "^2017-06" as disturbance
# OTHER: can start at any date. From 5-year "steady state" start, use "^2011-04"
recov <- time_series[,c(first(grep("^2017-06", colnames(time_series))):ncol(time_series))]

#full time series:
#recov <- time_series

### Adding dummy data to time series of recovery (for months with no observations)
# getting all months from beginning to end of recovery:
allmonths <- as.character(
  seq.Date(from = as.Date(first(names(recov))),
           to = as.Date(last(names(recov))),
           by="month"))
#selecting months with no observations:
no_obs_mos <- symdiff(names(recov), allmonths)
#make dummy dataframe with correct dimensions:
no_obs <- data.frame(matrix(data = NA, nrow = nrow(recov), ncol = length(no_obs_mos)))
#add the column names to sort by date during cbind:
colnames(no_obs) <- c(no_obs_mos)
#cbind for time series:
full_ts <- cbind(recov, no_obs)[order(c(names(recov),names(no_obs)))]

#add data for identifying site:
sites <- rep(1:nrow(full_ts), each = ncol(full_ts))
# make the data long:
pivot_ts <- pivot_longer(full_ts, cols = everything())
#add the site ID:
pivot_ts$site <- sites

##GGPLOT VERSION ----------------------
##turn this stuff on and change name to save:
#fname = "Harvard Forest/HFR_Raw_Time_Series_All_Sites_TCG_2023_07_27.tiff"
#tiff(fname, units = "in", width=12, height=4, res=300)
ggplot(data = pivot_ts, mapping = aes(x = as.Date(name), y = as.numeric(value/1000), 
                                      group = site, color = factor(site))) +
  geom_line(size=0.5) +
  geom_point() + 
  geom_line(data = filter(pivot_ts, is.na(value)==FALSE), 
            linetype = "dashed", size=0.3) +
  # geom_vline(xintercept = as.Date("2017-06-15"),
  #            linetype = "dashed", size=0.5) +
  ylim(c(0, 0.45)) + #for tcg version
  labs(title = "Post-Defoliation Recovery",
       y="Forest Condition Score",
       #y="Tasseled Cap Greenness Index Value", #or TCG - remember to change when making plots
       x="Time (Months)",
       color = "Site") +
  # scale_x_continuous(sec.axis = sec_axis(~.,
  #   name="Months Since Disturbance")) +
  #scale_x_continuous(breaks = seq(min(x), max(x), by = 5)) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
  
# theme(panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank())
#dev.off()



# R PLOT VERSION:---------------------
# #samp_time_series <- tcgjune[site,]
# #par(mar=c(1,1,1,1))
# plot(x, y, frame=FALSE,
#      xlab = "Growing Season Month",
#      ylab = "Condition Score",
#      pch = 20,
#      col = "forestgreen")#,
#      #ylim = c(-6.75,4.5))
# #lines(x, y, lwd = 1, col="forestgreen")

