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
librarian::shelf(googledrive, tidyverse, lubridate, ggtext, cowplot)

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
  # Select just June averages for best disturbance visual without seasonality
  dplyr::select(dplyr::contains(".06.")) %>%
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
  dplyr::filter(date > as.Date("2010-06-15")) %>%
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
  # Select just June averages for best disturbance visual without seasonality
  dplyr::select(dplyr::contains(".06.")) %>%
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
  dplyr::filter(date > as.Date("2010-06-15")) %>%
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

# Get color palette for HFR graphs
hfr_colors <- c("#001219", "#005f73","#4895ef", "#0a9396",  "#94d2bd", "#e9d8a6", 
                "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226", "#6a040f")

# Define the ggplot2 theme for all of these graphs
recovery_theme <- theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.line = element_line(colour = "black"))

# Make Tasseled Cap Greeness graph
tcg_series <- ggplot(data = tcg_v2, aes(x = date, y = tcg_value, color = sites)) +
  #Add background color for disturbance years:
  geom_rect(data=NULL, aes(xmin=as.Date("2016-05-01"), xmax=as.Date("2017-07-15"),
                           ymin=-Inf, ymax=Inf), alpha=0.2, show.legend = F,
            fill = "light grey", color = NA) +
  geom_line(aes(group = sites), linewidth = 0.5, show.legend = F,
            position = position_dodge(width = 0.2)) +
  geom_point(aes(fill = sites), pch = 21, size = 2.5, color = "black", alpha = 0.8,
             position = position_dodge(width = 0.2)) +
  # Add a dashed line connecting missing parts of each sites' time series
  geom_line(data = dplyr::filter(tcg_v2, !is.na(tcg_value)), 
            linetype = "dashed", linewidth = 0.3, show.legend = F,
            position = position_dodge(width = 0.2)) +
  # Vertical line at disturbance date
  geom_vline(xintercept = as.Date("2017-06-15"),
             linetype = "dashed", linewidth = 0.5) +
  # Y-axis limits
  lims(y = c(0, 0.45)) + 
  # Customize colors
  scale_color_manual(values = hfr_colors) +
  scale_fill_manual(values = hfr_colors) +
  # Custom axis labels
  labs(y = "Tasseled Cap Greenness\n Index Value", x = "Year", fill = "Site") +
  # Customize theme elements
  recovery_theme +
  theme(legend.position = "right",
        axis.title.y = element_text(size = 12.5),
        axis.title.x = element_text(size = 14)); tcg_series

# Make the same graph for condition score
cs_series <- ggplot(data = cs_v2, aes(x = date, y = cs_value, color = sites)) +
  #Add background color for disturbance years:
  geom_rect(data=NULL, aes(xmin=as.Date("2016-05-01"), xmax=as.Date("2017-07-15"),
                           ymin=-Inf, ymax=Inf), alpha=0.2, show.legend = F,
            fill = "light grey", color = NA) +
  geom_line(aes(group = sites), linewidth = 0.5, show.legend = F,
            position = position_dodge(width = 0.2)) +
  geom_point(aes(fill = sites), pch = 21, size = 2.5, color = "black", alpha = 0.8,
             position = position_dodge(width = 0.2)) +
  # Add a dashed line connecting missing parts of each sites' time series
  geom_line(data = dplyr::filter(cs_v2, !is.na(cs_value)), 
            linetype = "dashed", linewidth = 0.3, show.legend = F,
            position = position_dodge(width = 0.2)) +
  # Vertical line at disturbance date
  geom_vline(xintercept = as.Date("2017-06-15"),
             linetype = "dashed", linewidth = 0.5) +
  # Customize colors
  scale_color_manual(values = hfr_colors) +
  scale_fill_manual(values = hfr_colors) +
  # Custom axis labels
  labs(y = "Condition Score", x = "Year", fill = "Site") +
  # Customize theme elements
  recovery_theme +
  theme(axis.title = element_text(size = 14)); cs_series

# Generate a graph of NPP at CCE
npp_vs_age <- ggplot(npp_v2, aes(x = WaterAge, y = NPP, fill = 'x')) +
  geom_point(pch = 24, size = 2.5, alpha = 0.8) +
  geom_smooth(method = "loess", formula = "y ~ x", se = F, 
              color = "#bf812d", linewidth = 1.5) +
  # Custom axis labels (using markdown formatting to get formatting correct)
  labs(y = "Net Primary Production<br>mg C m<sup>-2</sup> / half photoperiod", 
       x =  "Water Parcel Age (days)<br>*sensu* Chabert *et al.*") +
  # Customize color / fill
  scale_fill_manual(values = "#35978f") +
  # Customize theme elements
  recovery_theme +
  theme(legend.position = "none",
        axis.title.x = ggtext::element_markdown(size = 15),
        axis.title.y = ggtext::element_markdown(size = 14.5)); npp_vs_age

# Herbivore biomass graph
herb_vs_age <- ggplot(herb_v2, aes(x = WaterAge, y = HerbivoreBiomass, fill = 'x')) +
  geom_point(pch = 22, size = 3.5, alpha = 0.8) +
  geom_smooth(method = "loess", formula = "y ~ x", se = F, 
              color = "#4d9221", linewidth = 2) +
  # Custom axis labels (using markdown formatting to get formatting correct)
  labs(y = "Herbivorous Metazoan Biomass<br>(mg C m<sup>-2</sup>)", 
       x =  "Water Parcel Age (days)<br>*sensu* Chabert *et al.*") +
  # Customize color / fill
  scale_fill_manual(values = "#c51b7d") +
  # Customize theme elements
  recovery_theme +
  theme(legend.position = "none",
        axis.title.x = ggtext::element_markdown(size = 15),
        axis.title.y = ggtext::element_markdown(size = 14.5)); herb_vs_age

## ------------------------------------------ ##
        # Figure Assembly & Export ----
## ------------------------------------------ ##

# Make a local folder for exporting figures
dir.create(path = file.path("_Manuscript Figures", "figure_files"), showWarnings = F)

# Identify the Drive folder finished figures should be uploaded to
recov_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1MKpJUKEjlwLF-pJ-wKhBbjDqSKLkyKuq")

# Check out the panel options
tcg_series
cs_series
npp_vs_age
herb_vs_age

# Make the two-panel graph (HFR + NPP from CCE)
(two_panel <- cowplot::plot_grid(tcg_series, npp_vs_age, ncol = 1, labels = "AUTO"))

# Generate a filepath/name for this figure
twopan_path <- file.path("_Manuscript Figures", "figure_files", 
                         "disturbance_recovery_2panel.png")

# Export locally
ggsave(filename = twopan_path, plot = two_panel, height = 9, width = 10, units = "in")

# Upload this file to the Drive
googledrive::drive_upload(media = twopan_path, overwrite = T, path = recov_drive)

# To make the three-panel graph, we need to make a 2-panel CCE graph first
(cce_fig <- cowplot::plot_grid(npp_vs_age, herb_vs_age, nrow = 1, labels = "AUTO"))

# Now we can add in the HFR graph of choice beneath these two
(three_panel <- cowplot::plot_grid(cce_fig, tcg_series, labels = c("", "C"), ncol = 1))

# Generate a file path object that includes a new file name
threepan_path <- file.path("_Manuscript Figures", "figure_files",
                           "disturbance_recovery_3panel.png")

# Export locally
ggsave(filename = threepan_path, plot = three_panel, height = 9, width = 10, units = "in")

# Upload this file to the Drive
googledrive::drive_upload(media = threepan_path, overwrite = T, path = recov_drive)

# End ----
