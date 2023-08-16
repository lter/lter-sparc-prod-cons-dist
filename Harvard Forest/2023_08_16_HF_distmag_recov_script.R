# files for condition score (cs) and tasseled cap greenness (tcg)
cs_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_score_mean.csv"
tcg_file <- "Harvard Forest/HF_2021_Paper_Data/2023_07_25_LTER_plots_monthly_tcg_mean.csv"
# load csvs:
cs <- read.csv(cs_file)
tcg <- read.csv(tcg_file)

magsrec1 <- spongy_mpr(tcg, cs, 2016)
magsrec2 <- spongy_mpr(tcg, cs, 2017)

magsrec3 <- spongy_mpr(cs, cs, 2016) #uses forest cond scores and initial dist yr 2016
magsrec4 <- spongy_mpr(cs, cs, 2017)

