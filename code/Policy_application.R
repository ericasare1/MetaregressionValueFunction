library(tidyverse)
dfcanada <- read_csv("data/Data_for_analysis_5_5_21.csv") %>% filter(us==0)
mean_income = mean(dfcanada$lninc)

sask_phjv <- read_csv("data/raw/phjv_sasK1.CSV")
colnames(sask_phjv)
view(sask_phjv)

sask_phjv <- sask_phjv %>%
  mutate(phjvloc = `PHJV Target Landscape`,
         numhh_phjv =  num_hhphvjmedianimput,
         numhh_outsidephjv = hhoutsidephjv, 
         avhhsize = av_hhszphvj, 
         lumpsum = 0,
         local = 1,
         forest =0,
         us = 0,
         volunt = 1,
         nrev = 1,
         lninc = mean_income,
         prov = 1,
         reg = 1,
         cult = 1,
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
    forest =0,
    us = 0,
    volunt = 1,
    nrev = 1,
    lninc = mean_income,
    prov = 1,
    reg = 1,
    cult = 1, 
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

# predicting wetland values
phjv_pred <- data.frame(phjv_fit=predict(Model_1c, newdata = sask_phjv, allow.new.levels = T)) %>%
  mutate(phjv_wtp = exp(phjv_fit),
         phjv_loc = sask_phjv$phjvloc,
         phjv_hhsz = sask_phjv$numhh_phjv,
         outsidephjv_hhsz = sask_phjv$numhh_outsidephjv,
         acre_change = sask_phjv$q0 - sask_phjv$q1)

outsidephjv_pred <- data.frame(fit=predict(Model_1c, newdata = sask_outsidephjv, allow.new.levels = T)) %>%
  mutate(outsidephjv_wtp = exp(fit),
         phjv_loc = sask_phjv$phjvloc)

phjv_pred %>% View()
outsidephjv_pred %>% View()

policy_df <- inner_join(phjv_pred,outsidephjv_pred, by = "phjv_loc")
view(policy_df)


#total wtp for locals and non local
totalWTP <- function(df){
  
  df <- df %>%
    mutate(totalWTPlocal = phjv_wtp * phjv_hhsz,
           totalWTPnonlocal = outsidephjv_wtp * outsidephjv_hhsz,
           totalWTPprovince = totalWTPlocal + totalWTPnonlocal)
  return(df)
}

policy_df <- totalWTP(df=policy_df)
write_csv(policy_df, "data/policy_df.csv")


#plot lnwtp vrs lnacres 
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)

policy_df %>% ggplot(aes(x=phjv_wtp, y = acre_change, label = phjv_loc)) + 
  geom_point(size= 3, alpha = 1) +
  labs(
    x = "Total Willingness to Pay ($/household)",
    y = "Wetland Acreage Change"
  ) +
  geom_label_repel(aes(label = phjv_loc),
                   box.padding   = 0.3, 
                   point.padding = 0.3,
                   size = 4,
                   segment.color = 'grey50') +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  theme(legend.position="bottom")
  
ggsave("output/policy_acres.png", width=11, height=8.5)

library(gtsummary)
library(flextable)
tbl_summary(
  policy_df,
  type = all_continuous() ~ "continuous2",
  statistic = all_continuous() ~ c( "{mean} ({sd})", 
                                    "{min}, {max}"),
  missing = "no"
) %>%
  add_n() %>% # add column with total number of non-missing observations
  #add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  as_flex_table() %>%
  save_as_docx(path = "output/policy_var.docx")
