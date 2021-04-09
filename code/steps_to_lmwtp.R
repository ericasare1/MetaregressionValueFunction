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

#Willingness to pay values extracted from original studies (C$ at study year)
tkac <- 146.98
trenholm_30mWoodlotRiparian <- 31.56
trenholm_60mWoodlotRiparian <- 19.38
trenholm_30mAllLandTypes <- 24.7
trenholm_60mAllLandTypes <- 14.7
pattisson <- 
pattisson <- 
pattisson <- 
pattisson <- 
pattisson <- 
pattisson <- 
lantz1 <- 
lantz2 <-
rudd1 <- 11.6
rudd2 <- 23.44
he_ce <- 482
he_cv <- 465
vossler <- 836

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
