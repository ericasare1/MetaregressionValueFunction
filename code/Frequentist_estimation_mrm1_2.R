
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,  brms, rstanarm, rstan, sjstats, lmerTest)

# Import data
#-----------------------------------------------
df <- read_csv("data/Data_for_analysis_11_4_21.csv")
str(df) #check the structure of variables

#function to scale variables to be between 0 and 1
normalized <- function(x) {
  (x- min(x))/(max(x) - min(x))
}

# Creating and transforming variables
df <- df %>%
  filter(wlfresh == 1) %>%
  mutate(lnq0 = log(q0+1), #plus 1 to prevent taking the log of zero
         lnq_change = log(q1-q0),
         lnwtp2 = lnwtp - lnq_change
  ) 

df %>% View()
df %>% filter(us == 1) %>% nrow() # number of observation of us studies
df %>% filter(us == 0) %>% nrow() # number of observation of canadian studies

#dataframe to create correlation map from more relevant model variables
df_cor <- df %>%  
  dplyr::select(lnwtp, lnwtp2, wtp_2017, q0, q1, lnyear, local, prov, reg, cult, lninc, forest, 
                volunt, lumpsum, ce, nrev, median, lnq0, lnq_change, us)


#----------------------------------------------Data Exploration------------------------
#A. checking the distribution of dependent variable
qqp(df$lnwtp, "norm") # 'QQP' gives the Quantile-Quantile Plot to compare the empirical
                      #q's and simulated q's from fitted beta distribution.

#B. Plot of distribution of lnwtp within study clusters:  .
df$id_study <- as.character(df$studyid) # create a character variable to identify obser. per study     
ggplot(df, aes(x= id_study, y = lnwtp, fill = as.character(us))) +
	geom_boxplot() +
	theme_bw(base_size = 14)

#C. checking for outliers
boxplot(df$lnwtp) 

#a)....Checking for multicollinearity with correlation map: Will use VIF to formally test it
#1. Correlation matrix
cormat <- round(cor(df_cor),2)
head(cormat)
#Reshape above matrix
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
#correlation heat map
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#lmtest::bptest(lm_restricted)  # Breusch-Pagan test
#-------------------Model Estimation : Results of Exploratory Exercise looks good to me

#Model 1: dep var is lnwtp and rel ind vars: lnqo and lnq_change
Model_1 <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
					 volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us +
					   (1 |studyid), data  = df)
summary(Model_1)
ranova(Model_1) 

#Model 2: dep var is lnwtp2 and rel ind vars: lnqo
Model_2 <- lmer(lnwtp2 ~ lnyear  + local + prov + reg + cult +forest + 
                  volunt + lumpsum + ce + nrev + lnq0 + us +
                  (1 |studyid), data  = df) #lninc dropped cos of multicollinearity
summary(Model_2)
ranova(Model_2) 

#Model Diagnostics
#checking if the random coefficient model is really significant

#Inter Class Correlation
performance::icc(Model_1) 
performance::icc(Model_2) 

#Checking for Multicollinearity
performance::check_collinearity(Model_1)
performance::check_collinearity(Model_2)

#c. checking for heteroscedasticity
#c1. Graphical way
plot(fitted((Model_1)), resid((Model_1), type = "pearson"))# this will create the plot
abline(0,0, col="red")

plot(fitted((Model_2)), resid((Model_2), type = "pearson"))# this will create the plot
abline(0,0, col="blue")

#c2. Statistical test
performance::check_heteroscedasticity(Model_1)
performance::check_heteroscedasticity(Model_2)

#Normality of residuals
qqnorm(resid(Model_1)) 
qqline(resid(Model_1), col = "red") # add a perfect fit line

#Model Performance
#a. Root mean squared error
performance::rmse(Model_1)
performance::rmse(Model_2) #lowest

#b. R square
performance::r2(Model_1)
performance::r2(Model_2) #better

#US-Canada model summary results
class(Model_1) <- "lmerMod"
class(Model_2) <- "lmerMod"

stargazer(Model_1, Model_2,
          type = "html",
          out="Us-Canada_models.doc",
          style = "qje",
          single.row = TRUE)
#----------------US study only models--------------------
df_us <- df %>% filter(us ==1)

#. Model 1
Model_1_us <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult  + forest + lninc +
							volunt + lumpsum + ce + lnq0 + lnq_change + (1 |studyid),
							data  = df_us)

ranova(Model_1_us) 
summary(Model_1_us)
performance::check_collinearity(Model_1_us)
performance::check_heteroscedasticity(Model_1_us)
performance::rmse(Model_1_us)
performance::r2(Model_1_us)

#. Model 2
Model_2_us <- lmer(lnwtp2 ~ lnyear  + local + prov + reg + cult  + forest + lninc +
                     volunt + lumpsum + ce + nrev + lnq0 + (1 |studyid),
                   data  = df_us)

ranova(Model_2_us) # mixed model not appropriate for the data: We model ordinary least squares

summary(Model_2_us)
performance::check_collinearity(Model_2_us)
performance::check_heteroscedasticity(Model_2_us)
performance::icc(Model_1_us)
performance::r2(Model_2_us)

#US only model summary results
class(Model_1_us) <- "lmerMod"
class(Model_2_us) <- "lmerMod"

stargazer(Model_1_us, Model_2_us,
          type = "html",
          out="Us_models.doc",
          style = "qje",
          single.row = TRUE)

# all models
stargazer(Model_1, Model_1_us, Model_2, Model_2_us,
          type = "html",
          out="all_models.doc",
          style = "qje",
          single.row = TRUE)

#............Transfer Error
set.seed(1200)
df_can <- df %>% filter(us==0)

#US-Canada model: Model 1 
df_prediction_alldata_m1 <- data.frame((predictInterval(merMod = Model_1, newdata = df_can,
                       level = 0.95, n.sims = 1000,
                       stat = "median", type="linear.prediction",
                       include.resid.var = TRUE))) #+ df$lnq_change : use this only for model 2
min(df_prediction_alldata$fit) # to check if there are negative predictions: must not be true for log-log
#There is no negative prediction so good to go

transfer_error_fulldata_m1 <- df_prediction_alldata_m1 %>%
  tibble(wtp = df_can$wtp_2017) %>%
  mutate(wtp_ypred = exp(fit) - 1,
         TE_MA = as.numeric((abs(wtp - wtp_ypred)/wtp)*100),
         TE_UnitTransfer = as.numeric((abs(wtp - mean(wtp))/wtp)*100),
         lowerC1_wtp = exp(lwr) - 1,
         upperC1_wtp = exp(upr) - 1)

transfer_error_fulldata %>% View()
mean(transfer_error_fulldata_m1$TE_MA)
mean(transfer_error_fulldata_m1$TE_UnitTransfer)
write_csv(transfer_error_fulldata_m1, "data/transfer_error_alldata_m1.csv")

#US-Canada model: Model 2 
df_prediction_alldata_m2 <- data.frame((predictInterval(merMod = Model_2, newdata = df_can,
                                                     level = 0.95, n.sims = 1000,
                                                     stat = "median", type="linear.prediction",
                                                     include.resid.var = TRUE))) %>%
  mutate(
    fit = fit + df_can$lnq_change) #+ df$lnq_change : use this only for model 2
min(df_prediction_alldata$fit) # to check if there are negative predictions: must not be true for log-log
#There is no negative prediction so good to go

transfer_error_fulldata_m2 <- df_prediction_alldata_m2 %>%
  tibble(wtp = df_can$wtp_2017) %>%
  mutate(wtp_ypred = exp(fit) - 1,
         TE_MA = as.numeric((abs(wtp - wtp_ypred)/wtp)*100),
         TE_UnitTransfer = as.numeric((abs(wtp - mean(wtp))/wtp)*100),
         lowerC1_wtp = exp(lwr) - 1,
         upperC1_wtp = exp(upr) - 1)

transfer_error_fulldata %>% View()
mean(transfer_error_fulldata_m2$TE_MA)
mean(transfer_error_fulldata_m2$TE_UnitTransfer)
write_csv(transfer_error_fulldata_m2, "data/transfer_error_alldata_m2.csv")


#US model when prediction data is us only data: Model 1 because it fit the data better than model 2
df_prediction_us <- data.frame((predictInterval(merMod = Model_2_us, newdata = df_us,
                                                     level = 0.95, n.sims = 1000,
                                                     stat = "median", type="linear.prediction",
                                                     include.resid.var = TRUE))) %>%
  mutate(fit = fit + df_us$lnq_change) #: use this only for model 2 cos dep var is lnwtp - lnq_change

min(df_prediction_us$fit) # to check if there are negative predictions: must not be true for log-log
#There is no negative prediction so good to go

transfer_error_us <- df_prediction_us %>%
  tibble(wtp = df_us$wtp_2017) %>%
  mutate(wtp_ypred = exp(fit) - 1,
         TE_MA = as.numeric((abs(wtp - wtp_ypred)/wtp)*100),
         TE_UnitTransfer = as.numeric((abs(wtp - mean(wtp))/wtp)*100),
         lowerC1_wtp = exp(lwr) - 1,
         upperC1_wtp = exp(upr) - 1,
         qchange = df_us$q1-df_us$q0)

transfer_error_us %>% View()
mean(transfer_error_us$TE_MA)
mean(transfer_error_us$TE_UnitTransfer)

write_csv(transfer_error_us, "data/transfer_error_us.csv")

#US model when prediction data is canada only data: Model 1 because it fit the data better than model 2
df_prediction_us_on_can <- data.frame((predictInterval(merMod = Model_2_us, newdata = df_can,
                                                level = 0.95, n.sims = 1000,
                                                stat = "median", type="linear.prediction",
                                                include.resid.var = TRUE))) %>%
  mutate(fit = fit + df_can$lnq_change) #: use this only for model 2 cos dep var is lnwtp - lnq_change

min(df_prediction_us_on_can$fit) # to check if there are negative predictions: must not be true for log-log
#There is no negative prediction so good to go

transfer_error_us_on_can <- df_prediction_us_on_can %>%
  tibble(wtp = df_can$wtp_2017) %>%
  mutate(wtp_ypred = exp(fit) - 1,
         TE_MA = as.numeric((abs(wtp - wtp_ypred)/wtp)*100),
         TE_UnitTransfer = as.numeric((abs(wtp - mean(wtp))/wtp)*100),
         lowerC1_wtp = exp(lwr) - 1,
         upperC1_wtp = exp(upr) - 1,
         qchange = df_can$q1-df_can$q0)

transfer_error_us_on_can %>% View()
mean(transfer_error_us_on_can$TE_MA)
mean(transfer_error_us_on_can$TE_UnitTransfer)

write_csv(transfer_error_us_on_can, "data/transfer_error_us_on_can.csv")

## -------------------Adding Up Test ------------
df
adding_up_testdata <- data.frame(
  scenario = c("F_SP_1_2","F_SP_2_3", "F_SP_1_3",
               "F_P_1_2","F_P_2_3", "F_P_1_3",
               "NF_SP_1_2","NF_SP_2_3", "NF_SP_1_3",
               "NF_P_1_2","NF_P_2_3", "NF_P_1_3"),
  forest = c(1,1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
  local = c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0),
  q0 = c(10000, 10030, 10000,10000, 10030, 10000, 10000, 10030, 10000, 10000, 10030, 10000),
  q1 = c(10030, 10050, 10050, 10030, 10050, 10050, 10030, 10050, 10050, 10030, 10050, 10050),
  list(studyid = sample(132:120, 12, replace = F)),
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
         lnyear = log(2018-1991) + 1,
         lnq0 = log(q0),
         lnq_change = log(q1- q0)
         )
adding_up_testdata %>% View()
#ading add Model 2 for the US model
adding_up_pred <- data.frame((predictInterval(merMod = Model_2_us, newdata = adding_up_testdata,
                                                level = 0.95, n.sims = 1000,
                                                stat = "median", type="linear.prediction",
                                                include.resid.var = T))) %>%
  mutate(fit = fit + adding_up_testdata$lnq_change,
         wtp = exp(fit)) 
write_csv(adding_up_pred, "data/addingup_model2_us.csv")
#Adding up: Model 2
adding_up_pred_model2 <- data.frame((predictInterval(merMod = Model_2, newdata = adding_up_testdata,
                                              level = 0.95, n.sims = 1000,
                                              stat = "median", type="linear.prediction",
                                              include.resid.var = T))) %>%
  mutate(fit = fit + adding_up_testdata$lnq_change,
         wtp = exp(fit)) 

adding_up_pred_model2 %>% View()

write_csv(adding_up_pred_model2, "data/addingup_model2.csv")

#Adding up: Model 1
adding_up_pred_model1 <- data.frame((predictInterval(merMod = Model_1, newdata = adding_up_testdata,
                                                     level = 0.95, n.sims = 1000,
                                                     stat = "median", type="linear.prediction",
                                                     include.resid.var = T))) %>%
  mutate(
         wtp = exp(fit)) 

adding_up_pred_model1 %>% View()

write_csv(adding_up_pred_model1, "data/addingup_model1.csv")

#bayesian
priors<-c(set_prior("normal(0,10)", class="b"),#prior for the beta's
		 set_prior("inv_gamma(.5,.5)", class="sigma"))#prior for the residual std. deviation

bayesian_mixed_rest = brm(
	lnwtp ~ lnq_change + (1 | studyid), 
	data  = df,
	prior = priors,
	cores = 4 
)

summary(bayesian_mixed_rest)

#Full
bayesian_mixed_full = brm(
	lnwtp ~ lnyear  +
		local + 
		prov + reg + cult + lninc +
		forest + 
		volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us + (1 | studyid), 
	data  = df,
	prior = priors,
	cores = 4,
	warmup = 1000, 
	iter = 5000,
	control = list(adapt_delta = 0.98))

bayesian_mixed_full_us = brm(
	lnwtp ~ lnyear  +
		local + 
		prov + reg + cult + lninc +
		forest + 
		volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us + (1 | studyid), 
	data  = df_us,
	prior = priors,
	cores = 4,
	warmup = 1000, 
	iter = 5000,
	control = list(adapt_delta = 0.98))

#Model Diasgnostics
#model fit
pp_check(bayesian_mixed_full)
pp_check(bayesian_mixed_full_us)
#covergence
plot(bayesian_mixed_full)
plot(bayesian_mixed_full_us)


summary(bayesian_mixed_full)
summary(bayesian_mixed_full_us)

#Model performance
performance::performance_rmse(bayesian_mixed_full) #compyes 
performance::performance_rmse(bayesian_mixed_full_us) #compyes 

## ..............Preictions using the PHJV data 
#1. Prepare data the data as we model data
df_pred <- read_csv("data/phjv_sask_2021.csv")  %>% 
	rename(volunt = volump) %>%
	mutate(lnq0 = log(q0+1),
		   us = 0,
		   diff_q = q1-q0) %>% View()
	group_by(studyid) %>%
	summarise(twtp = sum(diff_q)) %>% View()
	rename(location = "PHJV Target Landscape") 

#creating prediction data for the sub-provinces and total (for province)
df_pred_sk <- df_pred %>% filter(studyid ==133)
df_pred_ab <- df_pred %>% filter(studyid ==134)
df_pred_mn <- df_pred %>% filter(studyid ==135)
df_pred_sk_total <- df_pred %>% filter(studyid ==136) 
df_pred_ab_total <- df_pred %>% filter(studyid ==137)
df_pred_mn_total <- df_pred %>% filter(studyid ==138) 

#fitted_values <- data.frame(fitted(bayesian_mixed_full, newdata = df_allvs_sk, re_formula = ~ (1 | studyid))) 
prediction_all <-  data.frame(predict(bayesian_mixed_full, df_pred, allow_new_levels =TRUE))
prediction_sk <-  data.frame(predict(bayesian_mixed_full, df_pred_sk, allow_new_levels =TRUE)) 
prediction_ab <-  data.frame(predict(bayesian_mixed_full, df_pred_ab, allow_new_levels =TRUE))
prediction_mn <-  data.frame(predict(bayesian_mixed_full, df_pred_mn, allow_new_levels =TRUE))
prediction_sk_total <-  data.frame(predict(bayesian_mixed_full, df_pred_sk_total, allow_new_levels =TRUE)) 
prediction_ab_total <-  data.frame(predict(bayesian_mixed_full, df_pred_ab_total, allow_new_levels =TRUE))
prediction_mn_total <-  data.frame(predict(bayesian_mixed_full, df_pred_mn_total, allow_new_levels =TRUE)) 


full_pred <- tibble(studyid = df_pred$studyid, q1 = df_pred$q1,
					q0 = df_pred$q0, ln_change = df_pred$lnq_change,
					location = df_pred$location, lnwtp_predicted = prediction_all$Estimate) %>% 
	mutate(wtp = exp(lnwtp_predicted)) %>% 
	group_by(studyid) %>%
	summarise(total_wtp = sum(wtp)) %>% view()

#................Summary stats
## Vector of variables to summarize
myVars <- c("lnyear", "local",  "prov",  "reg",   "cult", "lninc",  "forest",
			"volunt",  "lumpsum",  "ce",  "nrev", "lnq_change", "us")

## Vector of categorical variables that need transformation
catVars <- c("local",  "prov",  "reg",   "cult", "forest",
			 "volunt",  "lumpsum",  "ce",  "nrev","us")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = df, factorVars = catVars)
print(tab2, showAllLevels = TRUE,  nonnormal = c("lnyear", "lninc"), exact = c("lnyear", "lninc")) #c("lumpsum",  "ce",  "nrev")
#summary results
stargazer(lm_restricted_mrm1,lm_restricted_mrm2, lm_full_mrm1, lm_full_mrm2,
		  type = "html",
		  out="mrm1_2.doc",
		  style = "qje",
		  single.row = TRUE)

class(mixed_full_mrm1) <- "lmerMod"
class(mixed_full_mrm2) <- "lmerMod"

stargazer(mixed_full_mrm1, mixed_full_mrm2,
		  type = "html",
		  out="mixed_models.doc",
		  style = "qje",
		  single.row = TRUE)

tidy(bayesian_mixed_full_us)

#nonlinear models
#. NLS

nlfunc <- function( ) {
	
	lnyear*b1  + local*b2 + prov*b3 + reg*b4 + cult*b5 + lninc*b6 + forest*b7 + 
		volunt*b8 + lumpsum*b9 + ce*b10 + nrev*b11 + lnq_change*b12 + 
		us*b13 + log((exp(b14 * q1) - exp(b14 * q0)) / b14)
	
}

ls_fit <- 
	
	
	
	
	
	
	
	
	