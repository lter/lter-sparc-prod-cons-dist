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

### MAKING THE ROC PLOTS FOR MORTALITY:

#set up data for plotting:
yvar <- hf_data$mort
xvar <- hf_data$mags

hf_gam <- gam(yvar ~ s(xvar), 
              data=hf_data,
              family = "binomial")

hf_roc<-roc(hf_gam$y,hf_gam$fitted.values)
fit <- hf_gam$fitted.values
#summ <- 

# making the plot ---
# gather data in new dataframe to eliminate duplicate columns error:
data <- data.frame(hf_data$mags/1000, hf_data$mort, fit)
names(data) <- c("mags", "mort", "fit")
#saving plots:
#tiff("2023_07_21_mort_bin_plot.tiff", units = "in", width=8, height=5, res=300)
#mortality binary plot:
ggplot(data=data, mapping = aes(x=mags, y=mort, color=mort,)) +
  geom_point(size=1.5, show.legend = FALSE) +
  scale_color_gradient(low="forestgreen", high="orange4") +
  geom_line(data = data, mapping = aes(x=mags, y=fit, color=fit),
            lwd=1.25, show.legend = FALSE) +
  scale_colour_gradient(low="forestgreen", high="orange4") +
  ggtitle("Tree Mortality vs Disturbance Magnitude") +
  ylab("Mortality (Binary)") +
  xlab("Disturbance Magnitude")
#dev.off()


### MAKING MORTALITY AND RECOVERY RATE VS % DEAD, % DEAD OAK:

#set up data for plotting:
yvar <- hf_data$percent_dead_oak_BA
xvar <- hf_data$mags
plot_data <- data.frame(xvar, yvar)

# run the gam:
hf_gam <- gam(yvar ~ s(xvar), 
              data=plot_data)
# get model fit and upper and lower confidence intervals:
pred <- predict_gam(hf_gam)
pred$lower <- pred$fit-qt(0.975,hf_gam$df.null)*pred$se.fit
pred$upper <- pred$fit+qt(0.975,hf_gam$df.null)*pred$se.fit

ggplot(plot_data, aes(xvar, yvar)) +
  geom_point() +
  geom_ribbon(data = pred, alpha=0.3,
              aes(y=fit, ymin = lower, ymax=upper), color=0) +
  geom_line(data = pred, aes(xvar, fit)) +
  xlim(c(0,max(xvar))) +
  ylim(c(min(yvar), max(yvar))) +
  labs(x="Disturbance Magnitude",
       y="Percent Dead Trees in Plot")



### MAKING BOXPLOTS FOR GROUPS OF DATA:

# data for plotting:
xvar <- hf_data$f.a[-168]
yvar <- hf_data$recov.rate[-168]
plot_data <- data.frame(xvar, yvar)

ggplot(data = plot_data, aes(x=factor(xvar), y=yvar)) +
  geom_boxplot() +
  labs(x="Ferns and Allies Presence/Absence",
       y="Recovery Rate") +
 # theme_bw() + 
  theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))


# data for plotting - seedling size classes data:
xvars <- hf_data[,c("seedlings_size_class_1",
                    "seedlings_size_class_2",
                    "seedlings_size_class_3")]
yvar <- hf_data$recov.rate
plot_data <- data.frame(xvars, yvar)

# divide into quantile groups:
plot_data <- plot_data %>%
  mutate(qgroup = ntile(xvar, 4))

ggplot(data = plot_data) + 
  geom_boxplot(data = plot_data, aes(x=xvars$seedlings_size_class_1, y=yvar))# +
  #geom_boxplot(data = plot_data, aes(x=xvars$seedlings_size_class_2, y=yvar)) +
  #geom_boxplot(data = plot_data, aes(x=xvars$seedlings_size_class_3, y=yvar))


### MAKING SAMPLE TIME SERIES PLOTS:

# Harvard Forest data - Landsat time series at some of the sites:
site <- 166
#tcg <- tcg.values
tcg <- cond.scores

#get just the tcg values from data frame:
tcgs<-tcg[,c(grep("^X",colnames(tcg)))]

y <- as.numeric((tcgs[site,32:80])/1000) #currently Apr 2011-Aug 2020
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
  score = y
)
colnames(sample_site) <- c("x","score")

##GGPLOT VERSION ----------------------
##turn this stuff on and change name to save:
#fname = "2023_07_20_Slow_Recov_sample_plot_166.tiff"
#tiff(fname, units = "in", width=12, height=3, res=300)
ggplot(data = sample_site, mapping = aes(x = x, y = score)) +
  geom_line(color="forestgreen") +
  geom_point(color="forestgreen") + 
  geom_line(data = filter(sample_site, is.na(score)==FALSE), 
            linetype = "dashed", color="forestgreen", size=0.3) +
  ylim(c(-2.2, 1.7)) +
  labs(title = "Sample Forest Condition(April 2011-August 2020): Slow Recovery", 
       y="Condition Score",
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


