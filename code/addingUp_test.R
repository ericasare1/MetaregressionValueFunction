

## -------------------Adding Up Test ------------

adding_up_testdata <- data.frame(
  scenario = c("F_SP_1_2","F_SP_2_3", "F_SP_1_3",
               "F_P_1_2","F_P_2_3", "F_P_1_3",
               "NF_SP_1_2","NF_SP_2_3", "NF_SP_1_3",
               "NF_P_1_2","NF_P_2_3", "NF_P_1_3"),
  
  forest = c(1,1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0), 
  local = c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0),
  q0 = c(10000, 10030, 10000,10000, 10030, 10000, 10000, 10030, 10000, 10000, 10030, 10000),
  q1 = c(10030, 10050, 10050, 10030, 10050, 10050, 10030, 10050, 10050, 10030, 10050, 10050),
  #list(studyid = sample(132:120, 12, replace = F)),
  us = 0)

adding_up_testdata <- adding_up_testdata %>%
  mutate(lumpsum = 0,
         volunt = 1,
         nrev = 1,
         lninc = mean(df$lninc),
         prov = 1, 
         reg = 1,
         cult = 0,
         ce = 1,
         lnyear = log(2017-1991 +1),
         lnq0 = log(q0),
         lnq_change = log(q1- q0),
         studyid = 0,
         studyid = ifelse(scenario == "F_SP_1_2" |scenario =="F_SP_2_3" | scenario =="F_SP_1_3", 126, studyid),
         studyid = ifelse(scenario == "F_P_1_2" |scenario =="F_P_2_3" |scenario =="F_P_1_3", 127, studyid),
         studyid = ifelse(scenario == "NF_SP_1_2"|scenario =="NF_SP_2_3"|scenario=="NF_SP_1_3", 128, studyid),
         studyid = ifelse(scenario == "NF_P_1_2"|scenario =="NF_P_2_3"|scenario=="NF_P_1_3", 129, studyid),
  )

adding_up_testdata %>% View()
#ading add Model 1 for the US-can model
adding_up_pred <- data.frame(fit=predict(Model_1c, newdata = adding_up_testdata, allow.new.levels = T)) %>%
  mutate(wtp = exp(fit)) 
adding_up_pred %>% View()
newpred <- data.frame(mod_us, newdata=test, allow.new.levels = T)

write_csv(adding_up_pred, "output/addingup_full.csv")

#Adding up: Model 2
adding_up_pred_us <- data.frame(fit=predict(Model_1c_us, newdata = adding_up_testdata, allow.new.levels = T)) %>%
  mutate(wtp = exp(fit)) 

adding_up_pred_us %>% View()
write_csv(adding_up_pred_us, "output/addingup_model_us.csv")



#(predictInterval(merMod = Model_1c_us, newdata = adding_up_testdata,
                # level = 0.95, n.sims = 1000,
                # stat = "median", type="linear.prediction",
               #  include.resid.var = T))