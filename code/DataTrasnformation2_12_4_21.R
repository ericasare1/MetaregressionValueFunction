#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Data Management: Extracting and creating final data for estimation<<<<<<<
# loading the required packages
if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)
}

# Load Packages
p_load(tidyverse, dplyr, summarytools, cansim)


#  >>>>>>>>>>>>>>>>> Conversions <<<<<<<<<<<<<<<<<<<<<
# Extracting consumer price index for Canada
cpi_canada <- get_cansim(1810000401) %>%
  filter(`Products and product groups` == "All-items",
         GEO == "Canada") %>%
  dplyr::select(REF_DATE, VALUE) %>%
  separate(REF_DATE, into = c('year', 'month'),  remove = F) %>%
  rename(cpi = VALUE) %>% 
  group_by(year) %>%
  summarise(cpi_annual = mean(cpi)) %>%
  filter(year > 2000) %>%
  column_to_rownames("year") 

#extracting the cpi for 2017.
avcpi_can_2017 <- cpi_canada["2017", "cpi_annual"]

#Creating the relative CPI dataframe
rel_cpi_can <- cpi_canada %>% rownames_to_column("year") %>% 
  mutate(rel_cpi = avcpi_can_2017/cpi_annual) %>% column_to_rownames("year")

#Extracting appropriate relative CPI for the study years for Canada studies
tkac <- rel_cpi_can["2001", "rel_cpi"]
trenholm <- rel_cpi_can["2007", "rel_cpi"]
pattisson <- rel_cpi_can["2008", "rel_cpi"]
lantz <- rel_cpi_can["2009", "rel_cpi"]
rudd <- rel_cpi_can["2011", "rel_cpi"]
he <- rel_cpi_can["2013", "rel_cpi"]
vossler <- rel_cpi_can["2014", "rel_cpi"]
dina <- rel_cpi_can["2003", "rel_cpi"]

#Converting the wtp estimates to 2017 Canadian prices (C$WTP/Household/Year)
#transformed_wtp <- us_canada %>%
 # dplyr::mutate(
 #   rel_cpi = ifelse(authors == "us", us_study_relcpi, 0),
 #   rel_cpi = ifelse(authors == "johnson_2016", johnson1_relcpi, rel_cpi),
 #   rel_cpi = ifelse(authors == "johnson_2015", johnson2_relcpi, rel_cpi),
  #  rel_cpi = ifelse(authors == "tkac_wtp", tkac, rel_cpi),
 #   rel_cpi = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
         #              authors == "trenholm_wtp_60mAll", trenholm, rel_cpi),
 #   rel_cpi = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                     #  authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", pattisson, rel_cpi),
   # rel_cpi = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", lantz, rel_cpi),
   # rel_cpi = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", rudd, rel_cpi),
  #  rel_cpi = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", he, rel_cpi),
  #  rel_cpi = ifelse(authors == "vossler_wtp", vossler, rel_cpi),
  #  wtp_2017 = rel_cpi * wtp_original,
  #  lnwtp = log(wtp_2017)) 

#Willingness to pay values extracted from original Canadian studies (C$ at study year)
tkac_wtp <- 79.22
trenholm_wtp_30W <- 31.56     #30m riparian buffer: woodlot
trenholm_wtp_60W<- 19.38      #60m riparian buffer: woodlot
trenholm_wtp_30mAll <- 24.7  #30m riparian buffer: all land types (woodlot, agric, residential)
trenholm_wtp_60mAll <- 14.7 #60m riparian buffer: all land types (woodlot, agric, residential)
pattisson_wtp_2008l <- 295.1        # Retention at 2008 level
pattisson_wtp_80 <- 301.65          # retention at 80% of 1968 level
pattisson_wtp_83 <- 308.31          # retention at 83% of 1968 level
pattisson_wtp_89 <- 321.46          # retention at 89% of 1968 level
pattisson_wtp_100 <- 347.78         # retention at 100% of 1968 level
lantz_wtp1 <- 0.3250*35 +  0.3467*222.2 +  0.3283*1060.7 # wetland retention 
lantz_wtp2 <- 0.3250*36  +  0.3467*227.5 +  0.3283*1038   # wetland retention plus restoration of additional 1000acres
rudd_wtp1 <- 47       # wetlands - some improvements
rudd_wtp2 <- 58      # wetlands - large improvements
he_wtp_ce <- 482               #estimate from choice experiment
he_wtp_cv <- 465               #estimate from contingent valuation
vossler_wtp <- 836
dina_wtp <- 62.4

#.......Income
#Extracting Income information from Canada Census 
# Download Statistics Canada data from Cansim by table name
# https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1110019001
inc_canada <- get_cansim(1110019001) 

#filters
unique(inc_canada$GEO) # "Quebec"  "Toronto, "New Brunswick"  "Manitoba" "Ontario" 
unique(inc_canada$`Income concept`)  #"Average after-tax income" 
unique(inc_canada$`Economic family type`) #"Economic families" 

inc_canada1 <- inc_canada%>%
  dplyr::filter(GEO =="New Brunswick"|GEO =="Ontario" ,
                `Income concept` == "Average total income",
                `Economic family type` == "Economic families and persons not in an economic family",
                REF_DATE==2007|REF_DATE==2009|REF_DATE==2011|REF_DATE==2014) %>%
  dplyr::select(REF_DATE, GEO, VALUE)

view(inc_canada1)
#%>% column_to_rownames("GEO")


 #Income
trenholm_inc <- inc_canada1  %>% filter(REF_DATE==2007 & GEO == "New Brunswick") %>% 
  dplyr::select(VALUE) %>% pluck(1)
lantz_inc <- inc_canada1  %>% filter(REF_DATE==2009 & GEO == "Ontario") %>% 
  dplyr::select(VALUE) %>% pluck(1)
rudd_inc <- inc_canada1  %>% filter(REF_DATE==2011 & GEO == "Ontario") %>% 
  dplyr::select(VALUE) %>% pluck(1)
vossler_inc <- inc_canada1  %>% filter(REF_DATE==2014 & GEO == "Quebec") %>% 
  dplyr::select(VALUE) %>% pluck(1)

tkac_inc <- 46858
pattisson_inc <- 57500    # Manitoba
he_inc <- 55313
vossler_inc <- (5+7+9+19*2+17+24)^-1 * (5*(15000) + 7*(.5*(15000+24999)) +
                                          9*(.5*(25000+34999)) + 19*(.5*(35000+54999)) + 
                                          19*(.5*(55000+74999)) + 17*(0.5*(75000+99000)) +
                                          24*(0.5*(100000)))
dina_inc <- 42685
johnson_inc <- (100)^-1 * (5*(10000) + 7*(.5*(10000+19999)) + 18*(.5*(20000+39999)) +   #Maine
                             19*(.5*(40000+59999)) + 17*(.5*(60000+79999)) + 
                             13*(0.5*(80000+99000)) + 20*(0.5*(100000+249)) + 3*(250000))
#creating income variable
#canada_data <- canada_data %>%
  #mutate(
  #  income = ifelse(authors == "tkac_wtp", tkac_inc, 0),
   # income  = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
            #           authors == "trenholm_wtp_60mAll", trenholm_inc, income),
    #income  = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
             #          authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", pattisson_inc, income),
 #   income  = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", lantz_inc, income),
 #   income  = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", rudd_inc, income),
 #   income  = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", he_inc, income),
 #   income  = ifelse(authors == "vossler_wtp", vossler_inc, income),
 #   income  = ifelse(authors == "dina_wtp", dina_inc, income)
 # )


#Constructing data.frame
canada_data <- data.frame(
  authors = c("tkac_wtp","trenholm_wtp_30W", "trenholm_wtp_60W", "trenholm_wtp_30mAll",
              "trenholm_wtp_60mAll", "pattisson_wtp_2008l","pattisson_wtp_80","pattisson_wtp_83",
              "pattisson_wtp_89", "pattisson_wtp_100","lantz_wtp1","lantz_wtp2","rudd_wtp1", "rudd_wtp2",
              "he_wtp_ce","he_wtp_cv", "vossler_wtp", "dina_wtp"),
  wtp_original = c(tkac_wtp,trenholm_wtp_30W, trenholm_wtp_60W, trenholm_wtp_30mAll,
                                      trenholm_wtp_60mAll, pattisson_wtp_2008l,pattisson_wtp_80, pattisson_wtp_83,
                                      pattisson_wtp_89, pattisson_wtp_100,lantz_wtp1,lantz_wtp2,rudd_wtp1, rudd_wtp2,
                                      he_wtp_ce,he_wtp_cv, vossler_wtp, dina_wtp)
                          ) %>%
    dplyr::mutate(
      
      #income
      income = ifelse(authors == "tkac_wtp", tkac_inc, 0),
      income  = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                         authors == "trenholm_wtp_60mAll", trenholm_inc, income),
      income  = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                         authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", pattisson_inc, income),
      income  = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", lantz_inc, income),
      income  = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", rudd_inc, income),
      income  = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", he_inc, income),
      income  = ifelse(authors == "vossler_wtp", vossler_inc, income),
      income  = ifelse(authors == "dina_wtp", dina_inc, income),
      
      #relative consumer price index
      rel_cpi = ifelse(authors == "tkac_wtp", tkac, 0),
      rel_cpi = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                         authors == "trenholm_wtp_60mAll", trenholm, rel_cpi),
      rel_cpi = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                         authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", pattisson, rel_cpi),
      rel_cpi = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", lantz, rel_cpi),
      rel_cpi = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", rudd, rel_cpi),
      rel_cpi = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", he, rel_cpi),
      rel_cpi = ifelse(authors == "vossler_wtp", vossler, rel_cpi),
      rel_cpi = ifelse(authors == "dina_wtp", vossler, rel_cpi),
      
      wtp_2017 = rel_cpi * wtp_original,
      lnwtp = log(wtp_2017),
      
      lninc = log(rel_cpi * income),
      
    
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
    year_study = ifelse(authors == "dina_wtp", 2003, year_study),
    
    #freshwater
    wlfresh = 1,
    
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
    studyid = ifelse(authors == "dina_wtp", 133, studyid),
    

    #log of year; oldest year is 1991 in the US data set
    lnyear = ifelse(authors == "tkac_wtp", log(2001 - 1991+1), 0),
    lnyear = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                       authors == "trenholm_wtp_60mAll", log(2007 - 1991 +1), lnyear),
    lnyear = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                       authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", log(2008 - 1991 +1), lnyear),
    lnyear = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", log(2009 - 1991 +1), lnyear),
    lnyear = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", log(2011 - 1991 +1), lnyear),
    lnyear = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", log(2013 - 1991 +1), lnyear),
    lnyear = ifelse(authors == "vossler_wtp", log(2014 - 1991 +1), lnyear),
    lnyear = ifelse(authors == "dina_wtp", log(2003 - 1991 +1), lnyear),
    
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
    local = ifelse(authors == "dina_wtp", 0, local),
    
    # provisioning, binary = 1 if wetland produced provisioning ess 0 otherwise
    prov = ifelse(authors == "tkac_wtp", 0, 1),
    prov = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                     authors == "trenholm_wtp_60mAll", 1, prov),
    prov = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                     authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, prov),
    prov = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, prov),
    prov = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 0, prov),
    prov = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 0, prov),
    prov = ifelse(authors == "vossler_wtp", 0, prov), 
    prov = ifelse(authors == "dina_wtp", 0, prov), 
    
    
    # regulation ess, binary = 1 if wetland produced regulation ess
    reg = ifelse(authors == "tkac_wtp", 1, 0),
    reg = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                    authors == "trenholm_wtp_60mAll", 1, reg),
    reg = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                    authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 1, reg),
    reg = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 1, reg),
    reg = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 1, reg),
    reg = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 1, reg),
    reg = ifelse(authors == "vossler_wtp", 1, reg), 
    reg = ifelse(authors == "dina_wtp", 0, reg), 
    
    # cultural ess, binary = 1 if wetland produced cultural ess
    cult = ifelse(authors == "tkac_wtp", 1, 0),
    cult = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                   authors == "trenholm_wtp_60mAll", 1, cult),
    cult = ifelse(authors == "pattisson_wtp_2008l"|authors == "pattisson_wtp_80"|authors == "pattisson_wtp_83"|
                   authors == "pattisson_wtp_89"|authors == "pattisson_wtp_100", 0, cult),
    cult = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, cult), 
    cult = ifelse(authors == "rudd_wtp1"|authors == "rudd_wtp2", 0, cult),
    cult = ifelse(authors == "he_wtp_ce"|authors == "he_wtp_cv", 0, cult),
    cult = ifelse(authors == "vossler_wtp", 0, cult), 
    cult = ifelse(authors == "dina_wtp", 1, cult), 
    
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
    forest = ifelse(authors == "dina_wtp", 1, forest), 
    
    
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
    q0 = ifelse(authors == "lantz_wtp1", 11997, q0), #11,997acres of wetlands would would remain by 2020
    q0 = ifelse(authors == "lantz_wtp2", 11997, q0), #11,997acres of wetlands would would remain by 2020
    q0 = ifelse(authors == "rudd_wtp1", 1307159, q0),
    q0 = ifelse(authors == "rudd_wtp2", 1307159, q0),
    q0 = ifelse(authors == "he_wtp_ce", 988422, q0),
    q0 = ifelse(authors == "he_wtp_cv", 988422, q0),
    q0 = ifelse(authors == "vossler_wtp", 0.14*2.9653E+8, q0), #out of a total of 1.2mill km2 society is mandated to retain 14%: 1.2mill km2 = 2.9653E+8
    q0 = ifelse(authors == "dina_wtp", 0, q0), 
    
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
    q1 = ifelse(authors == "lantz_wtp1", 17520, q1), #17520 acres of wetlands would would remain by 2020
    q1 = ifelse(authors == "lantz_wtp2", 18520, q1), #  #17520 acres of wetlands would would remain by 2020 + 1000 acres restored wetlands
    q1 = ifelse(authors == "rudd_wtp1", 1413400, q1),
    q1 = ifelse(authors == "rudd_wtp2", 1616100, q1),
    q1 = ifelse(authors == "he_wtp_ce", 1976843, q1),
    q1 = ifelse(authors == "he_wtp_cv", 1976843, q1),
    q1 = ifelse(authors == "vossler_wtp", 0.35*2.9653E+8, q1), #conservation target of 35% (inspired by ecologist)
    q1 = ifelse(authors == "dina_wtp", 225000 * 2.471, q1), 

    #volunt, binary = 1 if payment mechanism is voluntary
    volunt = 0,
    volunt = ifelse(authors == "tkac_wtp", 1, 0),
    volunt = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, volunt), #check
    volunt = ifelse(authors == "vossler_wtp", 1, volunt), #check
    volunt = ifelse(authors == "dina_wtp", 1, volunt), #check
    
    #lumpsun, binary = 1 if payment frequecy is once
    lumpsum = 1,
    lumpsum = ifelse(authors == "tkac_wtp", 1, 0),
    lumpsum = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, lumpsum), 
    lumpsun = ifelse(authors == "vossler_wtp", 1, lumpsum), 
    lumpsun = ifelse(authors == "dina_wtp", 0, lumpsum), 
    
    #nrev, binary = 1 paper is peer-reviewed
    nrev = 1,
    nrev = ifelse(authors == "tkac_wtp", 0, nrev),
    nrev = ifelse(authors == "dina_wtp", 0, nrev), 
    
    #median, binary = 1 if wtp is median value
    median = 0,
    median = ifelse(authors == "trenholm_wtp_30W"|authors == "trenholm_wtp_60W"|authors == "trenholm_wtp_30mAll"|
                      authors == "trenholm_wtp_60mAll", 1, 0),
    median = ifelse(authors == "lantz_wtp1"|authors == "lantz_wtp2", 0, median), 
    median = ifelse(authors == "vossler_wtp", 0, median),
    median = ifelse(authors == "dina_wtp", 0, median),
    
    
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
    ce = ifelse(authors == "dina_wtp", 0, ce),
    
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
    province = ifelse(authors == "dina_wtp", "SK", province),
    
  )

canada_data %>% View()


#<<<<<<<<<<<<<<<<<<<<<<<<<<< Additional US studies from Johnson et al (1 and 2) <<<<<<<<<<<<<<<<<<
#Johnson, Holland and Yao (2016). Individualized Geocoding in SP Questionnaires: 
                                  #Impl. for survey design and welfare est.
johnson_2016 <- 1.09 * (4000/47)    # wtp is 1.09 per 47 vegetated acres: 4000 acres 85% of original 4700 acres
#Johnson, Feurt and Holland (2015). Ecosystem serv and riparian land management in the Merriland, Branch Brook and Little River Watershed
johnson_2015 <- 0.044 * (200)    # wtp is 0.044 per 47 vegetated acres: 4000 acres 85% of original 4700 acresme
# 2017 us canada exchange rate: https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/
excrate_2017 = 1.3 

#<-------------- Extracting the CPI data for US
# Import data 
#-----------------------------------------------
cpi_us <- read_csv("data/raw/cpi_usa.csv") # this loads the consumer price index for US: 
cpi_us <- read_csv("data/cpi_us_csv.csv")

rel_cpi_us <- cpi_us %>% 
  column_to_rownames("year")

# CPI for 2017
avcpi_us_2017 <- rel_cpi_us["2017", "cpi"]

#Making the Relative CPI Data  for the US
rel_cpi_us <- rel_cpi_us %>% rownames_to_column("year") %>%
  mutate(rel_cpi = avcpi_us_2017/cpi) %>% column_to_rownames("year")

#Extracting appropriate relative CPI for the study years for both US and Canada studies
us_study_relcpi <- rel_cpi_us["2017", "rel_cpi"]
johnson_2016_relcpi <- rel_cpi_us["2016", "rel_cpi"]
johnson_2015_relcpi <- rel_cpi_us["2015", "rel_cpi"]

# Constructing additional us data 
add_us_data <- data.frame(
  authors = c("johnson_2016", "johnson_2015"),
  wtp_original = c(johnson_2016, johnson_2015))
add_us_data <- add_us_data %>% 
  dplyr::mutate(
    
    wtp_2017 = ifelse(authors == "johnson_2016", wtp_original*johnson_2016_relcpi,1),
    wtp_2017 = ifelse(authors == "johnson_2015", wtp_original*johnson_2015_relcpi,wtp_2017),
    lnwtp = log( wtp_2017),
    
    lninc = log(johnson_inc * 1.3 * johnson_2015_relcpi), #convert us income to can$
    #year study was conducted
    year_study = ifelse(authors == "johnson_2016", 2016, 0),
    year_study = ifelse(authors == "johnson_2015", 2015, year_study),   
    #creating studyid to identify clusters of studies
    studyid = ifelse(authors == "johnson_2016", 133, 0),
    studyid = ifelse(authors == "johnson_2015", 134, studyid),
    #creating US dummy
    us = 1,
    
    #Creating binary variable = 1 if water is freshwater
    wlfresh =  1,
    #log of year; oldest year is 1991 in the US data set
    lnyear = ifelse(authors == "johnson_2016", log(2016 - 1991 +1), 0),
    lnyear = ifelse(authors == "johnson_2015", log(2015 - 1991+ 1), lnyear),
    
    # local: binary = 1 if study is at the subprovince or state
    local = ifelse(authors == "johnson_2016", 1, 0),
    local = ifelse(authors == "johnson_2015", 1, local),
    # provisioning, binary = 1 if wetland produced provisioning ess 0 otherwise
    prov = ifelse(authors == "johnson_2016", 1, 0),
    prov = ifelse(authors == "johnson_2015", 1, prov),
    # regulation ess, binary = 1 if wetland produced regulation ess
    reg = ifelse(authors == "johnson_2016", 1, 0),
    reg = ifelse(authors == "johnson_2015", 0, reg),
    # cultural ess, binary = 1 if wetland produced cultural ess
    cult = ifelse(authors == "johnson_2016", 0, 0),
    cult = ifelse(authors == "johnson_2015", 0, cult),
    # forest, binary = 1 if wetland is in forest landscape
    forest = ifelse(authors == "johnson_2016", 0, 0),
    forest = ifelse(authors == "johnson_2015", 1, forest),
    # baseline acreage
    q0 = ifelse(authors == "johnson_2016", 4000, 0),
    q0 = ifelse(authors == "johnson_2015", 4000, q0),
  
    # policy acreage
    q1 = ifelse(authors == "johnson_2016", 4700, 0),
    q1 = ifelse(authors == "johnson_2015", 4700, q1),    
    #volunt, binary = 1 if payment mechanism is voluntary
    volunt = 0,
    volunt = ifelse(authors == "johnson_2016", 0, 0),
    volunt = ifelse(authors == "johnson_2015", 0, volunt),

    #lumpsun, binary = 1 if payment frequecy is once
    lumpsum = 1,
    lumpsum = ifelse(authors == "johnson_2016", 0, 0),
    lumpsum = ifelse(authors == "johnson_2015", 0, lumpsum), 
    
    #nrev, binary = 1 paper is peer-reviewed
    nrev = 1,
    nrev = ifelse(authors == "johnson_2016", 1, 0),
    nrev = ifelse(authors == "johnson_2015", 0, nrev),

    #median, binary = 1 if wtp is median value
    median = 0,
    median = ifelse(authors == "johnson_2016", 0, 0),
    median = ifelse(authors == "johnson_2015", 0, median), 

    # choice experiment, binary = 1 SP is choice experiment
    ce = 0,
    ce = ifelse(authors == "johnson_2016", 1, ce),
    ce = ifelse(authors == "johnson_2015", 1, ce),
    #study province
    province = "Maine",

  )
add_us_data %>% View()

#Original US data (Klaus)
us_data <- read_csv("data/metadata.csv") %>% filter(canada==0) %>% 
  mutate(us = 1, 
         authors = "klaus",
         wtp_original = exp(lnwtp), # use exp to transform lnwtp and convert to C$ with 2017 us-can exc rate
         
         wtp_2017 = wtp_original*us_study_relcpi *1.3,
         
         lnwtp = log( wtp_2017),
         
         lninc = log(exp(lninc) * 1.3)) %>%  
  filter(canada == 0) %>%
  dplyr::select(authors, studyid, lnwtp, wtp_original, wtp_2017, lnyear, lninc, local, prov, 
                reg, cult, forest, q0, q1, volunt, lumpsum, ce, nrev, median, us, wlfresh)
us_data %>% View()

#selecting relevant columns for model estimation
canada_data <- canada_data %>%
  mutate(us = 0) %>%
  dplyr::select(authors, studyid, lnwtp, wtp_original, wtp_2017, lnyear, lninc, local, 
         prov, reg, cult, forest, q0, q1, volunt, lumpsum, ce, nrev, median, us, wlfresh)

add_us_data <- add_us_data %>% 
  mutate(lnwtp = 0, us=1) %>%
  dplyr::select(authors, studyid, lnwtp, wtp_original, wtp_2017, lnyear, lninc, local,
         prov, reg, cult, forest, q0, q1, volunt, lumpsum, ce, nrev, median, us, wlfresh)

us_canada <- canada_data %>%
  add_row(add_us_data) %>%
  add_row(us_data) %>% mutate(lnwtp = log(wtp_2017))

us_canada %>% filter(wlfresh==1) %>% View()
                                            
view(us_canada)

write.csv(us_canada, "data/Data_for_analysis_5_5_21.csv")  # Final US-Canada data for estimations.

