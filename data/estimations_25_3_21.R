
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,  brms, rstanarm, rstan, sjstats, lmerTest)

# Import data
#-----------------------------------------------
df1 <- read_csv("data/Data_for_analysis_15_10.csv")
str(df) #check the structure of variables

#function to scale variables to be between 0 and 1
normalized <- function(x) {
	(x- min(x))/(max(x) - min(x))
}

# Creating variables
df2 <- df1 %>%
	filter(wlfresh == 1) %>%
	mutate(
		   lnq0 = log(q0+1),
		   q_ave = (q1 + q0)/2,  
		   q_diff = q1-q0,
		   q_percent = ifelse(q0 == 0, 100, 100*(q1 / q0 -1)), 
		   lnq_change = log(q1-q0),
		   lnwtp2 = lnwtp - log(q1- q0),
		   wtp = (exp(lnwtp) - 1),
		   lnwtp3 = wtp/q_diff,
		   us = ifelse(canada == 1, 0, 1)
	)
# Variable Diagnostics
#A. checking the distribution of dependent variable
qqp(df2$lnwtp, "norm")

#B. checking for outliers
boxplot(df2$lnwtp) 

# Model Estimation
#A. OLS
#1. With only the log of the change in baseline and post improvement wetland acres 
lm_restricted1 <- lm(lnwtp ~ lnq_change + lnq0, data= df2)
lm_restricted2 <- lm(lnwtp2 ~ lnq0, data= df2)

summary(lm_restricted1)
summary(lm_restricted2)

#b. Full OLS Model
lm_full1 <- lm(lnwtp ~ lnyear  + lninc + us + local + prov + reg + cult + forest + 
		   	volunt + lumpsum + ce + nrev + lnq_change + lnq0, data  = df2)
lm_full2 <- lm(lnwtp2 ~ lnyear  + lninc + us + local + prov + reg + cult + forest + 
		   	volunt + lumpsum + ce + nrev + lnq0, data  = df2)
summary(lm_full1)

#OLS Model Diagnostics
#I. Graphical Approach To Check for  fit
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_restricted1, las = 1)
#II. Statistical testing: Test of Homoscedasticity 
lmtest::bptest(lm_full1)  # Breusch-Pagan test
lmtest::bptest(lm_full2)  # Breusch-Pagan test

#B. Mixed Model: Random Coefficient Model 
#1. Restricted - Random intercept model
mixed_restricted1 = lmer(lnwtp ~ lnq_change + lnq0 + (1 | studyid), data= df2)
mixed_restricted2 = lmer(lnwtp2 ~ lnq0 + (1 | studyid), data= df2)

summary(mixed_restricted1)
summary(mixed_restricted2)

performance::icc(mixed_restricted) 

#1. Full - Random intercept model (...)
#2. Random intercept model (MRM1 in Moeltner et al. 2019)
mixed_full1 <- lmer(lnwtp ~ lnyear + lninc + us + local + prov + reg + cult + forest + 
						volunt + lumpsum + ce + nrev + lnq_change + lnq0 + (1 |studyid), data  = df2)

#3. Random intercept model (MRM2 in Moeltner et al. 2019)...
mixed_full2 <- lmer(lnwtp2 ~ lnyear + lninc + us + local + prov + reg + cult + forest + 
						volunt + lumpsum + ce + nrev + lnq0 + (1 |studyid), data  = df2)

mixed_full2_noinc <- lmer(lnwtp2 ~ lnyear + us + local + prov + reg + cult + forest + 
						volunt + lumpsum + ce + nrev + lnq0 + (1 |studyid), data  = df2)

#Model Diagnostics
#checking if the random coefficient model is really significant
ranova(mixed_restricted1) 
ranova(mixed_restricted2) 
ranova(mixed_full1) 
ranova(mixed_full2) 

#Inter Class Correlation
performance::icc(mixed_full1) 
performance::icc(mixed_full2) 

#Checking for Multicollinearity
performance::check_collinearity(mixed_full1)
performance::check_collinearity(mixed_full2)
performance::check_collinearity(mixed_full2_noinc)

#show estimated results
summary(mixed_full1)
summary(mixed_full2)
summary(mixed_full2_noinc)

#c. checking for heteroscedasticity
#c1. Graphical way
plot(fitted(mixed_full1), resid(mixed_full1, type = "pearson"))# this will create the plot
plot(fitted(mixed_full2), resid(mixed_full2, type = "pearson"))# this will create the plot
plot(fitted(mixed_full2_noinc), resid(mixed_full2_noinc, type = "pearson"))# this will create the plot
abline(0,0, col="red"
	   )
#c2. Statistical test
performance::check_heteroscedasticity(mixed_full1)
performance::check_heteroscedasticity(mixed_full2)
performance::check_heteroscedasticity(mixed_full2_noinc)

#Normality of residuals
qqnorm(resid(mixed_full1)) 
qqline(resid(mixed_full1), col = "red") # add a perfect fit line

#Model Performance
#a. Root mean squared error
performance::rmse(mixed_full1) #lowest
performance::rmse(mixed_full2)
performance::rmse(mixed_full2_noinc)

#b. R square
performance::r2(mixed_full)
performance::r2(mixed_full_mrm1) #better
performance::r2(mixed_full_mrm2)

#>>>>>>>>>>>>>>>>> US Analysis <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#creating data for US only studies
df_us <- df2 %>% filter(us ==1)

#1) Income variable dropped cos of singularity as a result of multicollinearity
mixed_full1_noinc_us <- lmer(lnwtp ~ lnyear + local + prov + reg + cult + forest + 
						  	volunt + lumpsum + ce + nrev + lnq_change + lnq0 + (1 |studyid), data  = df_us)
mixed_full2_noinc_us <- lmer(lnwtp2 ~ lnyear + local + prov + reg + cult + forest + 
							volunt + lumpsum + ce + nrev + lnq0  + (1 |studyid), data  = df_us)

ranova(mixed_full1_noinc_us)  # testing for the appropriateness of mixed model
ranova(mixed_full2_noinc_us) 

#Ordinary least squares cos Mixed model not appropriate
lm_full1_noinc_us <- lm(lnwtp ~ lnyear + local + prov + reg + cult + forest + 
							 	volunt + lumpsum + ce + nrev + lnq_change + lnq0, data  = df_us)
lm_full2_noinc_us <- lm(lnwtp2 ~ lnyear + local + prov + reg + cult + forest + 
							 	volunt + lumpsum + ce + nrev + lnq0, data  = df_us)
lmtest::bptest(lm_full1_noinc_us)  # Breusch-Pagan test
lmtest::bptest(lm_full2_noinc_us)  # Breusch-Pagan test


#PI <- predictInterval(merMod = mixed_full_mrm1, newdata = df,
					 # level = 0.95, n.sims = 1000,
					 # stat = "median", type="linear.prediction",
					 # include.resid.var = TRUE)
##1. Mixed model with robust standard errors

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Bayesian <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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
	lnwtp ~ lnyear + lninc + us + local + prov + reg + cult + forest + 
		volunt + lumpsum + ce + nrev + lnq_change + lnq0 + (1 |studyid), 
	data  = df2,
	prior = priors,
	cores = 4,
	warmup = 1000, 
	iter = 5000,
	control = list(adapt_delta = 0.98))

#US
bayesian_mixed_full_us = brm(
	lnwtp2 ~ lnyear + local + prov + reg + cult + forest + 
		volunt + lumpsum + ce + nrev + lnq0, 
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

#Model performance
performance::performance_rmse(bayesian_mixed_full) #compyes 
performance::performance_rmse(bayesian_mixed_full_us) #compyes 

#............Transfer Error
summary(mixed_full1)
df_can <- df2 %>% filter(us==0)
df_prediction <-  data.frame(Estimate = predict(mixed_full1, df_can, allow_new_levels =TRUE)) 

transfer_error_fulldata <- df_prediction %>%
	tibble(lnwtp = df_can$lnwtp) %>%
	rename(lnwtp_pred = Estimate) %>%
	mutate(wtp_y = exp(lnwtp) - 1,
		   wtp_ypred = exp(lnwtp_pred) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))
mean(transfer_error_fulldata$TE_MA)
mean(transfer_error_fulldata$TE_BT)

#-----US Transfer error
df_prediction_us <-  data.frame(Estimate = predict(lm_full2_noinc_us, df_can, allow_new_levels =TRUE)) 

transfer_error_fulldata_us <- df_prediction_us %>%
	tibble(lnwtp = df_can$lnwtp) %>%
	rename(lnwtp_pred = Estimate) %>%
	mutate(wtp_y = exp(lnwtp) - 1,
		   wtp_ypred = exp(lnwtp_pred) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))
mean(transfer_error_fulldata_us$TE_MA)
mean(transfer_error_fulldata_us$TE_BT)

## ..............Preictions using the PHJV data 
#1. Prepare data the data as we model data
df_pred <- read_csv("data/phjv_sask_2021.csv")  %>% 
	rename(volunt = volump) %>%
	mutate(lnq0 = log(q0+1),
		   us = 0,
		   diff_q = q1-q0) 
group_by(studyid) %>%
	summarise(twtp = sum(diff_q)) 
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
#summary result

class(mixed_restricted1) <- "lmerMod"
class(mixed_restricted2) <- "lmerMod"
class(mixed_full1) <- "lmerMod"
class(mixed_full2) <- "lmerMod"
class(mixed_full2_noinc) <- "lmerMod"
class(mixed_full1_noinc_us) <- "lmerMod"
class(mixed_full2_noinc_us) <- "lmerMod"

#combined data
stargazer(mixed_restricted1, mixed_restricted2, mixed_full1, mixed_full2, mixed_full2_noinc, 
		  type = "html",
		  out="mixed_models_25_3_21.doc",
		  style = "qje",
		  single.row = TRUE)
summary(mixed_full1)
#us
class(mixed_full1_noinc_us) <- "lmerMod"
class(mixed_full2_noinc_us) <- "lmerMod"
lm_full1_noinc_us 
lm_full2_noinc_us 

summary(lm_full2_noinc_us)
stargazer(mixed_full1_noinc_us, mixed_full2_noinc_us, lm_full1_noinc_us, lm_full2_noinc_us,
		  type = "html",
		  out="us_mixed_models_25_3_21.doc",
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
	
	
	
	
	
	
	
	
	