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
library(tidygam)
library(lattice)
library(tactile)


### MAKING SAMPLE TIME SERIES PLOTS:

# loading Landsat-based remote sensing data for forest condition scores (cs) and Tasseled Cap Greenness Index (tcg):
cs_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_score_mean.csv"
tcg_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_tcg_mean.csv"
# load csvs:
cs <- read.csv(cs_file)
tcg <- read.csv(tcg_file)

# Harvard Forest data - Landsat time series at some of the sites:
site <- 12 # 12 sites in data collection, pick 1-12
obs <- cs # data observations - tcg or cs file from above - tcg for TCG, cs for condition scores

#get just the tcg values from data frame:
time_series<-obs[,c(grep("^X",colnames(obs)))]

#remove unnecessary parts of colnames to just get dates:
colnames(time_series) <- str_replace_all(names(time_series), c("X"="", 
                                                               "_score_mean"="", 
                                                               "_tcg_mean"="",
                                                               "\\."="-"))

#y <- as.numeric(time_series[site,31:80]) #currently Apr 2011-Aug 2020
#y <- recov <- time_series[site,c(first(grep("^2017", colnames(time_series))):ncol(time_series))]
recov <- time_series[site,c(first(grep("^2017", colnames(time_series))):ncol(time_series))]

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

#x <- 1:length(y)
# TO DO: NEED TO ADD NON-DATA MONTHS TO TIME SERIES 

sample_site <- data.frame(
  month = colnames(full_ts),
  value = as.numeric(full_ts/1000) # corrects for scaling in data from GEE product
)
colnames(sample_site) <- c("x","value")

##GGPLOT VERSION ----------------------
##turn this stuff on and change name to save:
#fname = "2023_07_20_Slow_Recov_sample_plot_166.tiff"
#tiff(fname, units = "in", width=12, height=3, res=300)
ggplot(data = sample_site, mapping = aes(x = as.Date(x), y = value)) +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") + 
  geom_line(data = filter(sample_site, is.na(value)==FALSE), 
            linetype = "dashed", color="forestgreen", size=0.3) +
  #ylim(c(-3.2, 2)) +
  labs(title = "Sample Forest Condition: Recovery",
       y="Condition Score", #or TCG - remember to change when making plots
       x="Growing Season Month") +
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

