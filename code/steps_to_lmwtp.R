if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools, cansim)

# Import data 
#-----------------------------------------------
cpi_us <- read_csv("data/raw/cpi_usa.csv") # this loads the consumer price index for US: 
cpi_can <- read_csv("data/raw/cpi_canada.csv") 
exch_rate_us_to_can <- read_csv("data/raw/exc_us_to_can.csv")
wtp_orginal <- read_csv("data/raw/wtp_raw.csv")
meta_data <- read_csv("data/Data_for_analysis_15_10.csv") 
meta_data %>% View()
colnames(meta_data)

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
#Johnson, Holland and Yao (2016). Individualized Geocoding in SP Questionnaires: Impl. for survey design and welfare est.
johnson1_wtp_85 <- 1.09 * (4000/47)    # wtp is 1.09 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
johnson1_wtp_87 <- 1.09 * (4100/47)    # wtp is 1.09 per 47 vegetated acres: 4100 acres 87% of original 4700 acres
johnson1_wtp_90 <- 1.09 * (4200/47)    # wtp is 1.09 per 47 vegetated acres: 4200 acres 90% of original 4700 acres
johnson1_wtp_95 <- 1.09 * (4500/47)    # wtp is 1.09 per 47 vegetated acres: 4500 acres 95% of original 4700 acres

#Johnson, Feurt and Holland (2015). Ecosystem serv and riparian land management in the Merriland, Branch Brook and Little River Watershed
johnson2_wtp_85 <- 0.044 * (4000/47)    # wtp is 0.044 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
johnson2_wtp_87 <- 0.044 * (4100/47)    # wtp is 0.044 per 47 vegetated acres: 4100 acres 87% of original 4700 acres
johnson2_wtp_95 <- 0.044 * (4500/47)    # wtp is 0.044 per 47 vegetated acres: 4500 acres 95% of original 4700 acres

#Constructing data.frame
canada_data <- data.frame(
  authors = c("tkac_wtp","trenholm_wtp_30W", "trenholm_wtp_60W", "trenholm_wtp_30mAll",
              "trenholm_wtp_60mAll", "pattisson_wtp_2008l","pattisson_wtp_80","pattisson_wtp_83",
              "pattisson_wtp_89", "pattisson_wtp_100","lantz_wtp1","lantz_wtp2","rudd_wtp1", "rudd_wtp2",
              "he_wtp_ce","he_wtp_cv", "vossler_wtp"),
  wtp_original = c(tkac_wtp,trenholm_wtp_30W, trenholm_wtp_60W, trenholm_wtp_30mAll,
                                      trenholm_wtp_60mAll, pattisson_wtp_2008l,pattisson_wtp_80, pattisson_wtp_83,
                                      pattisson_wtp_89, pattisson_wtp_100,lantz_wtp1,lantz_wtp2,rudd_wtp1, rudd_wtp2,
                                      he_wtp_ce,he_wtp_cv, vossler_wtp)
                          ) %>%
   dplyr::mutate(
    us = 0,
    #year study was conducted
    year_study = ifelse(authors == "tkac_wtp", 2001, 0),
    year_study = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                       authors == "trenholm_wtp_60mAll", 2007, year_study),
    year_study = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                       authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 2008, year_study),
    year_study = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 2009, year_study),
    year_study = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 2011, year_study),
    year_study = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 2013, year_study),
    year_study = ifelse(authors == "vossler_wtp", 2014, year_study),
    #creating studyid to identify clusters of studies
    studyid = ifelse(authors == "tkac_wtp", 126, 0),
    studyid = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                       authors == "trenholm_wtp_60mAll", 127, studyid),
    studyid = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                       authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 128, studyid),
    studyid = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 129, studyid),
    studyid = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 130, studyid),
    studyid = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 131, studyid),
    studyid = ifelse(authors == "vossler_wtp", 132, studyid),
    #Creating binary variable = 1 if water is freshwater
    wlfresh =  1,
    #log of year; oldest year is 1991 in the US data set
    lnyear = ifelse(authors == "tkac_wtp", log(2001 - 1991) +1, 0),
    lnyear = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                       authors == "trenholm_wtp_60mAll", log(2007 - 1991) +1, lnyear),
    lnyear = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                       authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", log(2008 - 1991) +1, lnyear),
    lnyear = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", log(2009 - 1991) +1, lnyear),
    lnyear = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", log(2011 - 1991) +1, lnyear),
    lnyear = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", log(2013 - 1991) +1, lnyear),
    lnyear = ifelse(authors == "vossler_wtp", log(2014 - 1991) +1, lnyear),
    # local: binary = 1 if study is at the subprovince or state
    local = ifelse(authors == "tkac_wtp", 1, 0),
    local = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                      authors == "trenholm_wtp_60mAll", 1, local),
    local = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                      authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, local),
    local = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 1, local),
    local = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 0, local),
    local = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 1, local),
    local = ifelse(authors == "vossler_wtp", 0, local),
    # provisioning, binary = 1 if wetland produced provisioning ess 0 otherwise
    prov = ifelse(authors == "tkac_wtp", 1, 0),
    prov = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                     authors == "trenholm_wtp_60mAll", 1, prov),
    prov = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                     authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, prov),
    prov = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, prov),
    prov = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 1, prov),
    prov = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 0, prov),
    prov = ifelse(authors == "vossler_wtp", 1, prov), 
    
    # regulation ess, binary = 1 if wetland produced regulation ess
    reg = ifelse(authors == "tkac_wtp", 1, 0),
    reg = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                    authors == "trenholm_wtp_60mAll", 0, reg),
    reg = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                    authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 1, reg),
    reg = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 1, reg),
    reg = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 1, reg),
    reg = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 1, reg),
    reg = ifelse(authors == "vossler_wtp", 1, reg), 
    
    # cultural ess, binary = 1 if wetland produced cultural ess
    cult = ifelse(authors == "tkac_wtp", 0, 0),
    cult = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                   authors == "trenholm_wtp_60mAll", 0, cult),
    cult = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                   authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, cult),
    cult = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, cult), 
    cult = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 1, cult),
    cult = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 0, cult),
    cult = ifelse(authors == "vossler_wtp", 1, cult), 
    
    # forest, binary = 1 if wetland is in forest landscape
    forest = ifelse(authors == "tkac_wtp", 0, 0),
    forest = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                    authors == "trenholm_wtp_60mAll", 1, forest),
    forest = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                    authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, forest),
    forest = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, forest), 
    forest = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 1, forest),
    forest = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 0, forest),
    forest = ifelse(authors == "vossler_wtp", 1, forest), 
    
    # baseline acreage
    q0 = ifelse(authors == "tkac_wtp", 4200, 0),
    q0 = ifelse(authors == "trenholm_wtp_30W", 0, q0),
    q0 = ifelse(authors == "trenholm_wtp_60W", 0, q0),
    q0 = ifelse(authors == "trenholm_wtp_30mAll", 0, q0),
    q0 = ifelse(authors == "trenholm_wtp_60mAll", 0, q0),
    q0 = ifelse(authors == "pattisson_wtp_2008l", 949184, q0),
    q0 = ifelse(authors == "pattisson_wtp_80", 949184, q0),
    q0 = ifelse(authors == "pattisson_wtp_83", 949184, q0),
    q0 = ifelse(authors =="pattisson_wtp_89", 949184, q0),
    q0 = ifelse(authors == "pattisson_wtp_100", 949184, q0),
    q0 = ifelse(authors == "lantz_wtp1", 14520, q0), 
    q0 = ifelse(authors == "lantz_wtp2", 14520, q0), 
    q0 = ifelse(authors == "rudd_wtp1", 1307159, q0),
    q0 = ifelse(authors == "rudd_wtp2", 1307159, q0),
    q0 = ifelse(authors == "he_wtp_ce", 988422, q0),
    q0 = ifelse(authors == "he_wtp_cv", 988422, q0),
    q0 = ifelse(authors == "vossler_wtp", 29652645.8, q0), 
    
    # policy acreage
    q1 = ifelse(authors == "tkac_wtp", 8400, 0),
    q1 = ifelse(authors == "trenholm_wtp_30W", 5884, q1),
    q1 = ifelse(authors == "trenholm_wtp_60W", 11300, q1),
    q1 = ifelse(authors == "trenholm_wtp_30mAll", 7408, q1),
    q1 = ifelse(authors == "trenholm_wtp_60mAll", 14318, q1),
    q1 = ifelse(authors == "pattisson_wtp_2008l", 1044102, q1),
    q1 = ifelse(authors == "pattisson_wtp_80", 1084782, q1),
    q1 = ifelse(authors == "pattisson_wtp_83", 1125461, q1),
    q1 = ifelse(authors =="pattisson_wtp_89", 1206820, q1),
    q1 = ifelse(authors == "pattisson_wtp_100", 1355977, q1),
    q1 = ifelse(authors == "lantz_wtp1", 17520, q1), 
    q1 = ifelse(authors == "lantz_wtp2", 18520, q1), 
    q1 = ifelse(authors == "rudd_wtp1", 1413412, q1),
    q1 = ifelse(authors == "rudd_wtp2", 1413412, q1),
    q1 = ifelse(authors == "he_wtp_ce", 1976843, q1),
    q1 = ifelse(authors == "he_wtp_cv", 1976843, q1),
    q1 = ifelse(authors == "vossler_wtp", 129948359.5, q1), 
    
    #volunt, binary = 1 if payment mechanism is voluntary
    volunt = 0,
    volunt = ifelse(authors == "tkac_wtp", 1, 0),
    volunt = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, volunt), #check
    volunt = ifelse(authors == "vossler_wtp", 1, volunt), #check
    
    #lumpsun, binary = 1 if payment frequecy is once
    lumpsum = 1,
    lumpsum = ifelse(authors == "tkac_wtp", 0, 0),
    lumpsum = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, lumpsum), 
    lumpsun = ifelse(authors == "vossler_wtp", 0, lumpsum), 
    
    #nrev, binary = 1 paper is peer-reviewed
    nrev = 1,
    nrev = ifelse(authors == "tkac_wtp", 0, 0),
    nrev = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 1, nrev),
    nrev = ifelse(authors == "vossler_wtp", 1, nrev), 
    
    #median, binary = 1 if wtp is median value
    median = 0,
    median = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                      authors == "trenholm_wtp_60mAll", 1, 0),
    median = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, median), 
    median = ifelse(authors == "vossler_wtp", 0, median),
    
    # choice experiment, binary = 1 SP is choice experiment
    ce = 0,
    ce = ifelse(authors == "tkac_wtp", 0, ce),
    ce = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                    authors == "trenholm_wtp_60mAll", 0, ce),
    ce = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                    authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, ce),
    ce = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, ce), 
    ce = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 1, ce),
    ce = ifelse(authors == "he_wtp_ce", 1, ce),
    ce = ifelse(authors == "he_wtp_cv", 0, ce),
    ce = ifelse(authors == "vossler_wtp", 0, ce),
    
    #study province
    province = ifelse(authors == "tkac_wtp", "ON",0),
    province = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                  authors == "trenholm_wtp_60mAll", "NB", province),
    province = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                  authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", "MB", province),
    province = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", "ON", province), 
    province = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", "ON", province),
    province = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", "QE", province),
    province = ifelse(authors == "vossler_wtp", "QE", province),
  )

#Extracting 
# Download Statistics Canada data from Cansim by table name
# https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1110019001
cpi_sk <- get_cansim(1110019001) 

#filters
unique(cpi_sk$GEO) # "Quebec"  "Toronto, "New Brunswick"  "Manitoba" "Ontario" 
unique(cpi_sk$`Income concept`)  #"Average after-tax income" 
unique(cpi_sk$`Economic family type`) #"Economic families" 

cpi_sk1 <- cpi_sk %>%
  dplyr::filter(GEO == "Quebec"|GEO =="New Brunswick"| GEO =="Manitoba"|GEO =="Ontario" ,
         `Income concept` == "Average after-tax income",
         `Economic family type` == "Economic families",
         REF_DATE == 2017) %>%
  select(REF_DATE, GEO, VALUE) %>% 
  column_to_rownames("GEO")

inc_NB <- cpi_sk1["New Brunswick", "VALUE"]
inc_QE <- cpi_sk1["Quebec", "VALUE"]
inc_ON <- cpi_sk1["Ontario", "VALUE"]
inc_MB <- cpi_sk1["Manitoba", "VALUE"]

canada_data <- canada_data %>%
  mutate(
    lninc = ifelse(province == "NB", log(inc_NB), 0),
    lninc = ifelse(province == "QE", log(inc_QE), lninc),
    lninc = ifelse(province == "ON", log(inc_ON), lninc),
    lninc = ifelse(province == "MB", log(inc_MB), lninc)
  )
canada_data %>% View()

johnson1_wtp_85 <- 1.09 * (4000/47)    # wtp is 1.09 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
johnson1_wtp_87 <- 1.09 * (4100/47)    # wtp is 1.09 per 47 vegetated acres: 4100 acres 87% of original 4700 acres
johnson1_wtp_90 <- 1.09 * (4200/47)    # wtp is 1.09 per 47 vegetated acres: 4200 acres 90% of original 4700 acres
johnson1_wtp_95 <- 1.09 * (4500/47)    # wtp is 1.09 per 47 vegetated acres: 4500 acres 95% of original 4700 acres

#Johnson, Feurt and Holland (2015). Ecosystem serv and riparian land management in the Merriland, Branch Brook and Little River Watershed
johnson2_wtp_85 <- 0.044 * (4000/47)    # wtp is 0.044 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
johnson2_wtp_87 <- 0.044 * (4100/47)    # wtp is 0.044 per 47 vegetated acres: 4100 acres 87% of original 4700 acres
johnson2_wtp_95 <- 0.044 * (4500/47)

# Additional two USA studies
add_us_data <- data.frame(
  authors = c("johnson1_wtp_85","johnson1_wtp_87", "johnson1_wtp_90", "johnson1_wtp_95",
              "johnson2_wtp_85","johnson2_wtp_87", "johnson2_wtp_95"),
  wtp_original = c(johnson1_wtp_85,johnson1_wtp_87, johnson1_wtp_90, johnson1_wtp_95,
                   johnson2_wtp_85, johnson2_wtp_87, johnson2_wtp_95))
add_us_data <- add_us_data %>% 
  dplyr::mutate(
    #year study was conducted
    year_study = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                          authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 2015, 0),
    year_study = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                          authors == "johnson1_wtp_95", 2016, year_study),   
    #creating studyid to identify clusters of studies
    studyid = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                          authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 133, 0),
    studyid = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                          authors == "johnson1_wtp_95", 134, studyid),
    #creating US dummy
    us = 1,
  
    #Creating binary variable = 1 if water is freshwater
    wlfresh =  1,
    #log of year; oldest year is 1991 in the US data set
    lnyear = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                      authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", log(2015 - 1991) +1, 0),
    lnyear = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                      authors == "johnson1_wtp_95", log(2016 - 1991) + 1, lnyear),
    
    # local: binary = 1 if study is at the subprovince or state
    local = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                     authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 1, 0),
    local = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                     authors == "johnson1_wtp_95", 1, local),
    # provisioning, binary = 1 if wetland produced provisioning ess 0 otherwise
    prov = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                    authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 1, 0),
    prov = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                    authors == "johnson1_wtp_95", 1, prov),
    # regulation ess, binary = 1 if wetland produced regulation ess
    reg = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                   authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 1, 0),
    reg = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                   authors == "johnson1_wtp_95", 0, reg),
    # cultural ess, binary = 1 if wetland produced cultural ess
    cult = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                    authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 0, 0),
    cult = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                    authors == "johnson1_wtp_95", 0, cult),
    # forest, binary = 1 if wetland is in forest landscape
    forest = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                      authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 0, 0),
    forest = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                      authors == "johnson1_wtp_95", 1, forest),
    # baseline acreage
    q0 = ifelse(authors == "johnson1_wtp_85", 4200, 0),
    q0 = ifelse(authors == "johnson1_wtp_87", 0, q0),
    q0 = ifelse(authors == "johnson1_wtp_90", 0, q0),
    q0 = ifelse(authors == "johnson1_wtp_95", 0, q0),
    q0 = ifelse(authors == "johnson2_wtp_85", 0, q0),
    q0 = ifelse(authors == "johnson2_wtp_87", 0, q0),
    q0 = ifelse(authors == "johnson2_wtp_95", 0, q0),
  
    # policy acreage
    q1 = ifelse(authors == "johnson1_wtp_85", 4200, 0),
    q1 = ifelse(authors == "johnson1_wtp_87", 0, q1),
    q1 = ifelse(authors == "johnson1_wtp_90", 0, q1),
    q1 = ifelse(authors == "johnson1_wtp_95", 0, q1),
    q1 = ifelse(authors == "johnson2_wtp_85", 0, q1),
    q1 = ifelse(authors == "johnson2_wtp_87", 0, q1),
    q1 = ifelse(authors == "johnson2_wtp_95", 0, q1),    
    #volunt, binary = 1 if payment mechanism is voluntary
    volunt = 0,
    volunt = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                      authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 1, 0),
    volunt = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"
                    | authors == "johnson1_wtp_95", 0, volunt), #check

    #lumpsun, binary = 1 if payment frequecy is once
    lumpsum = 1,
    lumpsum = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                       authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 0, 0),
    lumpsum = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"
                     | authors == "johnson1_wtp_95", 0, lumpsum), 
    
    #nrev, binary = 1 paper is peer-reviewed
    nrev = 1,
    nrev = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                    authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 0, 0),
    nrev = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"
                  | authors == "johnson1_wtp_95", 1, nrev),

    #median, binary = 1 if wtp is median value
    median = 0,
    median = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                      authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 1, 0),
    median = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"
                    |authors == "johnson1_wtp_95", 0, median), 

    # choice experiment, binary = 1 SP is choice experiment
    ce = 0,
    ce = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"|
                  authors == "johnson1_wtp_90"| authors == "johnson1_wtp_95", 0, ce),
    ce = ifelse(authors == "johnson1_wtp_85"|authors == "johnson1_wtp_87"
                |authors == "johnson1_wtp_95", 0, ce),
    #study province
    province = "Maine",
    lninc = 0
  )

#selecting relevant columns
canada_data <- canada_data %>%
  mutate(lnwtp = 0,
         us = 0) %>%
  select(authors, studyid, lnwtp, wtp_original, lnyear, lninc, local, prov, reg, cult, forest, q0, q1, volunt, lumpsum, ce, nrev, median, us)

add_us_data <- add_us_data %>% 
  mutate(lnwtp = 0,
         us=1) %>%
  select(authors, studyid, lnwtp, wtp_original, lnyear, lninc, local, prov, reg, cult, forest, q0, q1, volunt, lumpsum, ce, nrev, median, us)

us_data <- meta_data %>%
  mutate(wtp_original = 0,
         us = 1,
         authors = "us") %>%
  filter(canada == 0) %>%
  select(authors, studyid, lnwtp, wtp_original, lnyear, lninc, local, prov, reg, cult, forest, q0, q1, volunt, lumpsum, ce, nrev, median)

us_canada <- canada_data %>%
  add_row(add_us_data) %>%
  add_row(us_data)

us_canada %>% View()

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
