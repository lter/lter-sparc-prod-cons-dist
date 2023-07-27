## ------------------------------------------ ##
#       Recovery from Disturbance Figure
## ------------------------------------------ ##
# Written by: Angel Chen

# PURPOSE
## Combining Mike's and Charlotte's plots into the same figure


## ------------------------------------------ ##
#               Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse)


## ------------------------------------------ ##
#             Download Data ----
## ------------------------------------------ ##
# Identify Mike's files to download
mikes_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1J1USxm8Qxhkkqhf20I2pp08b0LlxAowi"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("Herbivore v Water Age.csv", "NPP v Water Age.csv"))

# Create a folder to write these files to
dir.create(path = file.path("Figure Prep", "source_data"), showWarnings = F)

# Download these files into that folder
purrr::walk2(.x = mikes_files$id,
             .y = mikes_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x),
                                                path = file.path("Figure Prep", "source_data", .y),
                                                overwrite = T))
# Make a folder to export the figure in
dir.create(path = file.path("Figure Prep", "tidy_figure"), showWarnings = F)


## ------------------------------------------ ##
#             Read in Data ----
## ------------------------------------------ ##
# Point to Mike's folder
CCE_path <- file.path("Figure Prep", "source_data")

# Point to Charlotte's folder
HF_path <- file.path("Harvard Forest", "HF_2021_Paper_Data")

# Files for herbivore and NPP data
herb <- read.csv(file.path(CCE_path, "Herbivore v Water Age.csv"))
npp <- read.csv(file.path(CCE_path, "NPP v Water Age.csv"))

# Files for condition score (cs) and tasseled cap greenness (tcg)
cs <- read.csv(file.path(HF_path, "2023_07_25_LTER_plots_monthly_score_mean.csv"))
tcg <- read.csv(file.path(HF_path, "2023_07_25_LTER_plots_monthly_tcg_mean.csv"))


## ------------------------------------------ ##
#              Data Wrangling ----
## ------------------------------------------ ##
# NOTE: this section was adapted from 2023_07_25_HF_Figures_Script.R
# Harvard Forest data - Landsat time series at some of the sites:
obs <- tcg  # data observations - tcg or cs file from above - tcg for TCG, cs for condition scores

time_series <- obs %>%
  # Get just the tcg values from data frame:
  dplyr::select(starts_with("X")) %>% 
  # Remove unnecessary parts of the column names to just get dates:
  dplyr::rename_with(~str_replace_all(.x, 
    c("X"="", "_score_mean"="", "_tcg_mean"="", "\\."="-")),
    .cols = everything())

# Make sure column names are in the right YYYY-MM-DD format 
dplyr::glimpse(time_series)

### VERSION FOR PLOTTING ALL SITES - adding single line for each site: ------------
# make full time series with missing dummy variables -

# start with grabbing section of time series that you want to plot:
# FULL TIME SERIES recov object is just the time_series object (see below)
# RECOVERY WINDOW: "^2017-06" as disturbance
# OTHER: can start at any date. From 5-year "steady state" start, use "^2011-04"
recov <- time_series %>%
  dplyr::select(starts_with("2017-06"):last_col())

#full time series:
#recov <- time_series

dplyr::glimpse(recov)

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

## ------------------------------------------ ##
#             Figure Creation ----
## ------------------------------------------ ##

##GGPLOT VERSION ----------------------
##turn this stuff on and change name to save:
fname = file.path("Figure Prep", "tidy_figure", "HFR_Raw_Time_Series_All_Sites_TCG_2023_07_27.tiff")
tiff(fname, units = "in", width=12, height=4, res=300)
ggplot(data = pivot_ts, mapping = aes(x = as.Date(name), y = as.numeric(value/1000), 
                                      group = site, color = factor(site))) +
  geom_line(size=0.5) +
  geom_point() + 
  geom_line(data = filter(pivot_ts, is.na(value)==FALSE), 
            linetype = "dashed", size=0.3) +
  geom_vline(xintercept = as.Date("2017-06-15"),
             linetype = "dashed", size=0.5) +
  ylim(c(0, 0.45)) + #for tcg version
  labs(#title = "Post-Defoliation Recovery",
    y="Tasseled Cap Greenness Index Value", #or CS - remember to change when making plots
    x="Time (Months)",
    color = "Site") +
  #scale_x_continuous(breaks = seq(min(x), max(x), by = 5)) +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
# theme(panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank())
dev.off()

# need to incorporate Mike's plots in this...