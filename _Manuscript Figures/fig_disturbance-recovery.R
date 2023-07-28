## ------------------------------------------ ##
#       Recovery from Disturbance Figure
## ------------------------------------------ ##
# Written by: Angel Chen, Nick J Lyon

# PURPOSE
## Combining Mike's (CCE) and Charlotte's (HFR) plots into the same figure

## ------------------------------------------ ##
#               Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, lubridate)

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
#             Download Data ----
## ------------------------------------------ ##
# Identify Mike's files to download
mikes_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1J1USxm8Qxhkkqhf20I2pp08b0LlxAowi"), type = "csv") %>%
  # Filter to desired files
  dplyr::filter(name %in% c("Herbivore v Water Age.csv", "NPP v Water Age.csv"))

# Create a folder to write these files to
dir.create(path = file.path("_Manuscript Figures", "source_data"), showWarnings = F)

# Download these files into that folder
purrr::walk2(.x = mikes_files$id, .y = mikes_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), overwrite = T,
                                                path = file.path("_Manuscript Figures",
                                                                 "source_data", .y)))

## ------------------------------------------ ##
#             Read in Data ----
## ------------------------------------------ ##
# Point to Mike's folder
CCE_path <- file.path("_Manuscript Figures", "source_data")

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

# Process some information from the time series
tcg_v2 <- tcg %>%
  # Drop unwanted columns
  dplyr::select(dplyr::starts_with("X")) %>%
  # Generate site column
  dplyr::mutate(sites = as.factor(1:nrow(x = .)), .before = dplyr::everything()) %>%
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("X")) %>%
  # Strip out just the actual date
  dplyr::mutate(date_char = stringr::str_replace_all(string = name,
                                                     c("X|_score_mean|_tcg_mean" = "", 
                                                       "\\." = "-"))) %>%
  # Break out year and month information
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(string = date_char, start = 1, end = 4)),
    month = as.numeric(stringr::str_sub(string = date_char, start = 6, end = 7))) %>%
  # Get a real date column
  dplyr::mutate(date = as.Date(date_char)) %>%
  # Keep only after June of 2017
  dplyr::filter(date > as.Date("2017-06-15")) %>%
  # Divide tassled cap greenness (TCG) by 1000
  dplyr::mutate(tcg_value = value / 1000) %>%
  # Count months since disturbance
  dplyr::mutate(time_after = lubridate::interval(start = as.Date("2017-06-15"), end = date),
                months_after = floor(time_after / months(x = 1))) %>%
  # Pare down to only needed columns
  dplyr::select(sites, date, months_after, tcg_value)
  
# Check structure
dplyr::glimpse(tcg_v2)

# Wrangle the condition score data as well
cs_v2 <- cs %>%
  # Drop unwanted columns
  dplyr::select(dplyr::starts_with("X")) %>%
  # Generate site column
  dplyr::mutate(sites = as.factor(1:nrow(x = .)), .before = dplyr::everything()) %>%
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("X")) %>%
  # Strip out just the actual date
  dplyr::mutate(date_char = stringr::str_replace_all(string = name,
                                                     c("X|_score_mean|_tcg_mean" = "", 
                                                       "\\." = "-"))) %>%
  # Break out year and month information
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(string = date_char, start = 1, end = 4)),
    month = as.numeric(stringr::str_sub(string = date_char, start = 6, end = 7))) %>%
  # Get a real date column
  dplyr::mutate(date = as.Date(date_char)) %>%
  # Keep only after June of 2017
  dplyr::filter(date > as.Date("2017-06-15")) %>%
  # Divide condition score (CS) by 1000
  dplyr::mutate(cs_value = value / 1000) %>%
  # Count months since disturbance
  dplyr::mutate(time_after = lubridate::interval(start = as.Date("2017-06-15"), end = date),
                months_after = floor(time_after / months(x = 1))) %>%
  # Pare down to only needed columns
  dplyr::select(sites, date, months_after, cs_value)

# Re-check structure
dplyr::glimpse(cs_v2)

# Wrangle CCE net primary productivity data
npp_v2 <- npp %>%
  # Drop absent data
  dplyr::filter(!is.na(WaterAge) & !is.na(NPP)) %>%
  # Drop 'infinity' as a water age value
  dplyr::filter(WaterAge < Inf)

# Check structure
dplyr::glimpse(npp_v2)

# Wrangle herbivore biomass CCE data
herb_v2 <- herb %>%
  # Keep only data with values for both columns
  dplyr::filter(!is.na(WaterAge) & !is.na(HerbivoreBiomass))

# Check structure
dplyr::glimpse(herb_v2)

## ------------------------------------------ ##
            # Figure Creation ----
## ------------------------------------------ ##

# Define the ggplot2 theme for all of these graphs
recovery_theme <- theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.line = element_line(colour = "black"))

# Make Tasseled Cap Greeness graph
tcg_series <- ggplot(data = tcg_v2, aes(x = date, y = tcg_value, color = sites)) +
  geom_line(aes(group = sites), linewidth = 0.5) +
  geom_point() +
  # Add a dashed line connecting missing parts of each sites' time series
  geom_line(data = dplyr::filter(tcg_v2, !is.na(tcg_value)), 
            linetype = "dashed", linewidth = 0.3) +
  # Vertical line at disturbance date
  geom_vline(xintercept = as.Date("2017-06-15"),
             linetype = "dashed", linewidth = 0.5) +
  # Y-axis limits
  lims(y = c(0, 0.45)) + 
  # Custom axis labels
  labs(y = "Tasseled Cap Greenness Index Value", x = "Year", color = "Site") +
  # Customize theme elements
  recovery_theme +
  theme(axis.title.y = element_text(size = 12.5),
        axis.title.x = element_text(size = 14))

# Check that out
tcg_series

# Make the same graph for condition score
cs_series <- ggplot(data = cs_v2, aes(x = date, y = cs_value, color = sites)) +
  geom_line(aes(group = sites), linewidth = 0.5) +
  geom_point() +
  # Add a dashed line connecting missing parts of each sites' time series
  geom_line(data = dplyr::filter(cs_v2, !is.na(cs_value)), 
            linetype = "dashed", linewidth = 0.3) +
  # Vertical line at disturbance date
  geom_vline(xintercept = as.Date("2017-06-15"),
             linetype = "dashed", linewidth = 0.5) +
  # Custom axis labels
  labs(y = "Condition Score", x = "Year", color = "Site") +
  # Customize theme elements
  recovery_theme +
  theme(axis.title = element_text(size = 14))

# Look at that
cs_series

## ------------------------------------------ ##
# Figure Assembly & Export ----
## ------------------------------------------ ##

# Make a folder for local export of the figure
dir.create(path = file.path("_Manuscript Figures", "figure_files"), showWarnings = F)





# End ----

# BASEMENT ----

## Superseded code retained for posterity

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
# fname = file.path("Figure Prep", "tidy_figure", "HFR_Raw_Time_Series_All_Sites_TCG_2023_07_27.tiff")
# tiff(fname, units = "in", width=12, height=4, res=300)
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
# dev.off()


## ------------------------------------------ ##
# Figure Export ----
## ------------------------------------------ ##

# Make a folder for local export of the figure
dir.create(path = file.path("California Current Ecosystem", "tidy_figure"), showWarnings = F)


# need to incorporate Mike's plots in this...



