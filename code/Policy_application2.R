library(tidyverse)
library(cansim)

dfcanada <- read_csv("data/Data_for_analysis_5_5_21.csv") %>% filter(us==0)

sask_phjv <- read_csv("data/raw/phjv_sasK2.CSV")
colnames(sask_phjv)
view(sask_phjv)

canada_inc <- get_cansim(1110019001) 


inc_canada1 <- canada_inc%>%
  dplyr::filter(GEO =="Saskatchewan"|GEO =="Manitoba"|GEO == "Alberta",
                `Income concept` == "Average total income",
                `Economic family type` == "Economic families and persons not in an economic family",
                REF_DATE==2017) %>%
  dplyr::select(REF_DATE, GEO, VALUE)

view(inc_canada1)
#%>% column_to_rownames("GEO")


#Income
sask_inc <- inc_canada1  %>% filter(REF_DATE==2017 & GEO == "Alberta") %>% 
  dplyr::select(VALUE) %>% pluck(1)
alb_inc <- inc_canada1  %>% filter(REF_DATE==2017 & GEO == "Saskatchewan") %>% 
  dplyr::select(VALUE) %>% pluck(1)
man_inc <- inc_canada1  %>% filter(REF_DATE==2017 & GEO == "Manitoba") %>% 
  dplyr::select(VALUE) %>% pluck(1)


sask_phjv <- sask_phjv %>%
  mutate(phjvloc = `PHJV Target Landscape`,
         numhh_phjv =  num_hhphvjmedianimput,
         numhh_outsidephjv = hhoutsidephjv, 
         avhhsize = av_hhszphvj, 
         lumpsum = 0.06,
         local = 1,
         local = ifelse(province=='sk', 0, local),
         local = ifelse(province=='ab', 0, local),
         local = ifelse(province=='ma', 0, local),
         forest =0,
         us = 0,
         volunt = 0.17,
         nrev = 0.89,
         lninc = log(sask_inc),
         lninc = ifelse(province=='ab', log(alb_inc), lninc),
         lninc = ifelse(province=='ma', log(man_inc), lninc),
         prov = 1,
         reg = 1,
         cult = 1,
         ce = 0.17,
         lnyear = log(2017-1991 +1),
         lnq0 = log(q0),
         lnq_change = log(q0- q1),
         studyid = 130
                  ) 

write.csv(sask_phjv, "data/data_poli.csv")


# predicting wetland values
phjv_pred <- data.frame(phjv_fit=predict(Model_1d, newdata = sask_phjv, allow.new.levels = T)) %>%
  mutate(phjv_wtp = exp(phjv_fit),
         phjv_loc = sask_phjv$phjvloc,
         phjv_hhsz = sask_phjv$numhh_phjv,
         outsidephjv_hhsz = sask_phjv$numhh_outsidephjv,
         acre_change = sask_phjv$q0 - sask_phjv$q1,
         province = sask_phjv$province)
View(phjv_pred)

write_csv(phjv_pred, "data/policy_df.csv")


#plot lnwtp vrs lnacres 
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)

policy_graph <- phjv_pred %>% filter(province == 0)
mean(phjv_pred$phjv_wtp)

policy_graph %>% filter(acre_change < 11734) %>% ggplot(aes(x=phjv_wtp, y = acre_change, label = phjv_loc)) + 
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
  poli,
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




