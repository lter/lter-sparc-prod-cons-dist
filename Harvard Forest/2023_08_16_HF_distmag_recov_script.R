# files for condition score (cs) and tasseled cap greenness (tcg)
cs_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_score_mean.csv"
tcg_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_tcg_mean.csv"
# load csvs:
cs <- read.csv(cs_file)
tcg <- read.csv(tcg_file)