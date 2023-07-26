#Figures code for ESA 2023

#load libraries:
library(tidyverse)
library(ggplot2)
library(dplyr)
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
site <- 2 # 12 sites in data collection, pick 1-12
obs <- cs # data observations - tcg or cs file from above - tcg for TCG, cs for condition scores

#get just the tcg values from data frame:
time_series<-obs[,c(grep("^X",colnames(obs)))]

y <- as.numeric(time_series[site,],) #currently Apr 2011-Aug 2020
x <- 1:length(y)

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

sample_site <- data.frame(
  month = x,
  value = y/1000 # corrects for scaling in data from GEE product
)
colnames(sample_site) <- c("x","value")

##GGPLOT VERSION ----------------------
##turn this stuff on and change name to save:
#fname = "2023_07_20_Slow_Recov_sample_plot_166.tiff"
#tiff(fname, units = "in", width=12, height=3, res=300)
ggplot(data = sample_site, mapping = aes(x = x, y = value)) +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") + 
  geom_line(data = filter(sample_site, is.na(value)==FALSE), 
            linetype = "dashed", color="forestgreen", size=0.3) +
  ylim(c(-3.2, 2)) +
  labs(title = "Sample Forest Condition",
       y="Condition Score", #or TCG - remember to change when making plots
       x="Growing Season Month") +
  scale_x_continuous(breaks = seq(min(x), max(x), by = 5)) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank())
#dev.off()


#testing subset sites:
# distsites <- c(10,18,21,30,31,32,47,53,60,
#                66,72,97,98,101,102,105,110,
#                111,120,130,135,141,142,145,
#                146,151,152,153,157,160,170,
#                171,172,188,195,196,203)
# distsdoub <- c(14,119,147,161,164,166,167,173,174,175,191,197)
# fastrecov <- c(41,42,43,94,95)
# slowrecov <- c(48,54,55,58,75,87)
# 
# ds <- tcgs[distsites, 21:79]
# dd <- tcgs[distsdoub, 21:79]
# fr <- tcgs[fastrecov, 21:79]
# sr <- tcgs[slowrecov, 21:79]
# 
# ds_mr <- hf_data[distsites,]
# dd_mr <- hf_data[distsdoub,]
# fr_mr <- hf_data[fastrecov,]
# sr_mr <- hf_data[slowrecov,]


