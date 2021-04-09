if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools)

# Import data 
#-----------------------------------------------
cpi_us <- read_csv("data/raw/cpi_usa.csv") # this loads the consumer price index for US: 
cpi_can <- read_csv("data/raw/cpi_canada.csv") 
exch_rate_us_to_can <- read_csv("data/raw/exc_us_to_can.csv")
wtp_orginal <- read_csv("data/raw/wtp_raw.csv")
meta_data <- read_csv("data/Data_for_analysis_15_10.csv")

#Willingness to pay values extracted from original Canadian studies (C$ at study year)
tkac_wtp <- 146.98
trenholm_wtp_30W <- 31.56     #30m riparian buffer: woodlot
trenholm_wtp_60W<- 19.38      #60m riparian buffer: woodlot
trenholm_wtp_30mAll <- 24.7  #30m riparian buffer: all land types (woodlot, agric, residential)
trenholm_wtp_60mAll <- 14.7 #60m riparian buffer: all land types (woodlot, agric, residential)
pattisson_wtp_2008l <- 295.1        # Retention at 2008 level
pattisson_wtp_80 <- 301.65          # retention at 80% of 1968 level
pattisson_wtp_83 <- 308.31          # retention at 83% of 1968 level
pattisson_wtp_89 <- 321.46          # retention at 89% of 1968 level
pattisson_wtp_100 <- 347.78         # retention at 100% of 1968 level
lantz_wtp1 <- mean(35+222.2+1060.7) # wetland retention
lantz_wtp2 <- mean(36+227.5+1038)   # wetland retention plus restoration of additional 1000acres
rudd_wtp1 <- 11.6       # wetlands - some improvements
rudd_wtp2 <- 23.44      # wetlands - large improvements
he_wtp_ce <- 482               #estimate from choice experiment
he_wtp_cv <- 465               #estimate from contingent valuation
vossler_wtp <- 836

#willingness to pay estimates from additional US studies
#Johnson, Holland and Yao (2016). Individualised Geocoding in SP Questionnaires: Impl. for survey design and welfare est.
johnson1_wtp_85 <- 1.09 * (4000/47)    # wtp is 1.09 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
johnson1_wtp_87 <- 1.09 * (4100/47)    # wtp is 1.09 per 47 vegetated acres: 4100 acres 87% of original 4700 acres
johnson1_wtp_90 <- 1.09 * (4200/47)    # wtp is 1.09 per 47 vegetated acres: 4200 acres 90% of original 4700 acres
johnson1_wtp_95 <- 1.09 * (4500/47)    # wtp is 1.09 per 47 vegetated acres: 4500 acres 95% of original 4700 acres

#Johnson, Feurt and Holland (2015). Ecosystem serv and riparian land management in the Merriland, Branch Brook anf Little River Watershed
johnson2_wtp_85 <- 0.044 * (4000/47)    # wtp is 0.044 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
johnson2_wtp_87 <- 0.044 * (4100/47)    # wtp is 0.044 per 47 vegetated acres: 4100 acres 87% of original 4700 acres
johnson2_wtp_95 <- 0.044 * (4500/47)    # wtp is 0.044 per 47 vegetated acres: 4500 acres 95% of original 4700 acres

  
exchrate <- exch_rate_us_to_can %>% 
	group_by(Year) %>%
	summarize(av_monthly_excrate = mean(exchrate)) %>%
	column_to_rownames("Year") 

avcpi_us_2018 <- 105.9447
avcpi_can_2018 <- 104

rel_cpi_us <- cpi_us %>% 
	group_by(year) %>% 
	summarise(average_us_cpi = mean(us_cpi)) %>% 
	mutate(rel_cpi = avcpi_us_2018/average_us_cpi) %>%
	column_to_rownames("year") 

rel_cpi_can <- cpi_can %>% 
	group_by(year) %>% 
	summarise(average_can_cpi = mean(cpi_can))  %>%
	mutate(rel_cpi = avcpi_can_2018/average_can_cpi) %>%
	column_to_rownames("year") 

us_study_relcpi <- rel_cpi_us["2017", "rel_cpi"]
tkac <- rel_cpi_can["2001", "rel_cpi"]
trenholm <- rel_cpi_can["2007", "rel_cpi"]
pattisson <- rel_cpi_can["2008", "rel_cpi"]
lantz <- rel_cpi_can["2009", "rel_cpi"]
rudd <- rel_cpi_can["2011", "rel_cpi"]
he <- rel_cpi_can["2013", "rel_cpi"]
vossler <- rel_cpi_can["2014", "rel_cpi"]

exchrate_2018 <- exchrate["2018", "av_monthly_excrate"]

transformed_wtp <- wtp_orginal %>%
	mutate(rel_cpi = 0,
		   rel_cpi = ifelse(Author == "us_study", us_study_relcpi, rel_cpi),
		   rel_cpi = ifelse(Author == "tkac", tkac, rel_cpi),
		   rel_cpi = ifelse(Author == "trenholm", trenholm, rel_cpi),
		   rel_cpi = ifelse(Author == "pattisson", pattisson, rel_cpi),
		   rel_cpi = ifelse(Author == "lantz", lantz, rel_cpi),
		   rel_cpi = ifelse(Author == "rudd", rudd, rel_cpi),
		   rel_cpi = ifelse(Author == "he", he, rel_cpi),
		   rel_cpi = ifelse(Author == "vossler", vossler, rel_cpi),
		   wtp_original = ori_wtp_us_in_lg,
		   wtp_original = ifelse(Author == "us_study", exp(wtp_original)-1, wtp_original),
		   wtp_2018 = rel_cpi * wtp_original,
		   wtp_2018can = ifelse(Author == "us_study", wtp_2018*exchrate_2018, wtp_2018),
		   lnwtp = log(wtp_2018can)) %>% 
	select(obsid,lnwtp)

meta_data %>% select(-c(lnwtp)) %>% inner_join(transformed_wtp, by = "obsid") %>% 
	write.csv("data/Data_for_analysis_15_10.csv")
