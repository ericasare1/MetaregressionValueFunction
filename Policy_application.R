
dfcanada <- read_csv("data/Data_for_analysis_5_5_21.csv") %>% filter(us==0)
mean_income = mean(dfcanada$lninc)

sask_phjv <- read_csv("data/raw/phjv_sasK.CSV")
colnames(sask_phjv)

sask_phjv <- sask_phjv %>%
  mutate(phjvloc = `PHJV Target Landscape`,
         rm_pop = `rm pop`,
         lumpsum = 0,
         local = 1,
         forest =1,
         us = 0,
         volunt = 1,
         nrev = 1,
         lninc = mean_income,
         prov = 1,
         reg = 1,
         cult = 0,
         ce = 1,
         lnyear = log(2017-1991 +1),
         lnq0 = log(q0),
         lnq_change = log(q0- q1),
         studyid = 130
                  )
sask_outsidephjv <- sask_phjv %>%
  mutate(
    lumpsum = 0,
    local = 0,
    forest =1,
    us = 0,
    volunt = 1,
    nrev = 1,
    lninc = mean_income,
    prov = 1,
    reg = 1,
    cult = 0,
    ce = 1,
    lnyear = log(2017-1991 +1),
    lnq0 = log(q0),
    lnq_change = log(q0- q1),
    studyid = 130
  )
view(sask_phjv)
#studyid = ifelse(scenario == "F_SP_1_2" |scenario =="F_SP_2_3" | scenario =="F_SP_1_3", 126, studyid),
#studyid = ifelse(scenario == "F_P_1_2" |scenario =="F_P_2_3" |scenario =="F_P_1_3", 127, studyid),
#studyid = ifelse(scenario == "NF_SP_1_2"|scenario =="NF_SP_2_3"|scenario=="NF_SP_1_3", 128, studyid),
#studyid = ifelse(scenario == "NF_P_1_2"|scenario =="NF_P_2_3"|scenario=="NF_P_1_3", 129, studyid),
write_csv(sask_phjv, "data/sask_phjv.csv")

# predicting wetland values
phjv_pred <- data.frame(phjv_fit=predict(Model_1c, newdata = sask_phjv, allow.new.levels = T)) %>%
  mutate(phjv_wtp = exp(phjv_fit),
         phjv_loc = sask_phjv$phjvloc,
         rm_pop = sask_phjv$rm_pop)

outsidephjv_pred <- data.frame(fit=predict(Model_1c, newdata = sask_outsidephjv, allow.new.levels = T)) %>%
  mutate(outsidephjv_wtp = exp(fit),
         phjv_loc = sask_phjv$phjvloc)

phjv_pred %>% View()
outsidephjv_pred %>% View()

policy_df <- inner_join(phjv_pred,outsidephjv_pred, by = "phjv_loc")
view(policy_df)
