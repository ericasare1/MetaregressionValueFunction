library(tidyverse)
dfcanada <- read_csv("data/Data_for_analysis_5_5_21.csv") %>% filter(us==0) 
mean_income = mean(dfcanada$lninc)

inc_canada <- get_cansim(1110019001) 

#filters
unique(inc_canada$GEO) # "Quebec"  "Toronto, "New Brunswick"  "Manitoba" "Ontario" 
unique(inc_canada$REF_DATE)

inc_canada1 <- inc_canada%>%
  dplyr::filter(GEO =="Saskatchewan"|GEO =="Alberta" |GEO =="Manitoba",
                `Income concept` == "Average total income",
                `Economic family type` == "Economic families and persons not in an economic family",
                REF_DATE=="2017") %>%
  dplyr::select(REF_DATE, GEO, VALUE)

mean_inc_sk <- inc_canada1 %>% filter(GEO =="Saskatchewan") %>% dplyr::select(VALUE) %>% pluck()
mean_inc_ab <- inc_canada1 %>% filter(GEO =="Alberta") %>% dplyr::select(VALUE) %>% pluck()
mean_inc_mn <- inc_canada1 %>% filter(GEO =="Manitoba") %>% dplyr::select(VALUE) %>% pluck()


phjv_wlvalues <- tibble(
  studyloc = c("sk", "ab", "mn"),
  lninc = c(log(91100), log(107600), log(88500)),
  lnq0 = c(log(2762641), log(125076), log(339756)),
  lnq_change = c(log(112574), log(25648),log(11734)),
  local = 0,
  forest = 0,
  us = 0,
  lnyear = log(2017-1991 +1),
  studyid = 130,
  prov = mean(df$prov),
  lumpsum = mean(df$lumpsum),
  volunt = mean(df$volunt),
  nrev = mean(df$nrev),
  reg = mean(df$reg),
  cult = mean(df$cult),
  ce = mean(df$ce)
) 

phjv_wlvalues %>% View()
str(phjv_wlvalues)
write.csv(phjv_wlvalues, "data/newpolicy_provinces.csv")

# predicting wetland values
phjv_pred <- data.frame(phjv_fit=predict(Model_1d, newdata = phjv_wlvalues, allow.new.levels = T),
                        studyloc = c("sk", "ab", "mn")) %>%
  mutate(phjv_wtp = exp(phjv_fit),
         phjv_hhsz = ifelse(studyloc == "sk", 432652, 0),
         phjv_hhsz = ifelse(studyloc == "ab", 1527675, phjv_hhsz),
         phjv_hhsz = ifelse(studyloc == "mn", 489050, phjv_hhsz),
         totaleconlost = phjv_wtp*phjv_hhsz
         )
phjv_pred %>% View()
write_csv(phjv_pred, "data/policy_df_11_11.csv")


