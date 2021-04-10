
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

#-------------------Model Estimation : Results of Exploratory Exercise looks good to me
#A. OLS
#1. With only the log of the change in baseline and post improvement wetland acres 
lm_restricted <- lm(lnwtp ~ lnq_change, data= df)
summary(lm_restricted)

#b. Full OLS Model
lm_full <- lm(lnwtp ~ lnyear  +
			  	local + 
			  	prov + reg + cult + lninc +
			  	forest + 
			  	volunt + lumpsum + ce + nrev + lnq_change + us, data= df)
summary(lm_full)

#OLS Model Diagnostics
#I. Graphical Approach To Check for  fit
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm_restricted, las = 1)
#II. Statistical testing: Test of Homoscedasticity 
lmtest::bptest(lm_restricted)  # Breusch-Pagan test

#B. Mixed Model: Random Coefficient Model 
#1. Restricted - Random intercept model
mixed_restricted = lmer(lnwtp ~ lnq_change + (1 | studyid), data= df)
summary(mixed_restricted)
performance::icc(mixed_restricted) 

#1. Full - Random intercept model (...)
mixed_full <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
					 volunt + lumpsum + ce + nrev + lnq_change + us + (1 |studyid), data  = df)

#2. Random intercept model (MRM1 in Moeltner et al. 2019)
mixed_full_mrm1 <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
						volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us + (1 |studyid), data  = df)

#3. Random intercept model (MRM2 in Moeltner et al. 2019)...
mixed_full_mrm2 <- lmer(lnwtp2 ~ lnyear  + local + prov + reg + cult + lninc +forest + 
						volunt + lumpsum + ce + nrev + q01 + us + (1 |studyid), data  = df)

#4. Random intercept model (Normalised MRM1)...
mixed_full_mrm3 <- lmer(lnwtp2 ~ year_sc  + local + prov + reg + cult + income_sc +forest + 
							volunt + lumpsum + ce + nrev + q_diff_sc + us + (1 |studyid), data  = df)

#Model Diagnostics
#checking if the random coefficient model is really significant
ranova(mixed_full) 
ranova(mixed_full_mrm1) 
ranova(mixed_full_mrm2) 
ranova(mixed_full_mrm3) 

#Inter Class Correlation
performance::icc(mixed_full) 
performance::icc(mixed_full_mrm1) 
performance::icc(mixed_full_mrm2) 
performance::icc(mixed_full_mrm3) 

#Checking for Multicollinearity
performance::check_collinearity(mixed_full)
performance::check_collinearity(mixed_full_mrm1)
performance::check_collinearity(mixed_full_mrm2)
performance::check_collinearity(mixed_full_mrm3)

#show estimated results
summary(mixed_full)
summary(mixed_full_mrm1)
summary(mixed_full_mrm2)
summary(mixed_full_mrm3)

#c. checking for heteroscedasticity
#c1. Graphical way
plot(fitted(mixed_full), resid(mixed_full, type = "pearson"))# this will create the plot
abline(0,0, col="red")
#c2. Statistical test
performance::check_heteroscedasticity(mixed_full)
performance::check_heteroscedasticity(mixed_full_mrm1)
performance::check_heteroscedasticity(mixed_full_mrm2)
#Normality of residuals
qqnorm(resid(mixed_full)) 
qqline(resid(mixed_full), col = "red") # add a perfect fit line

#Model Performance
#a. Root mean squared error
performance::rmse(mixed_full)
performance::rmse(mixed_full_mrm1) #lowest
performance::rmse(mixed_full_mrm2)
performance::rmse(mixed_full_mrm3)

#b. R square
performance::r2(mixed_full)
performance::r2(mixed_full_mrm1) #better
performance::r2(mixed_full_mrm2)

# MRM2 fits the data better and also the coefficient on lnq_change has the right positive sign
#creating data for US only studies
df_us <- df %>% filter(us ==1) %>%
	mutate(lnq0_sc = log(q0_sc+1))

#. Random intercept model (MRM1 in Moeltner et al. 2019)
mixed_full_us <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult  + forest + 
							volunt + lumpsum + ce + nrev + lnq0 + lnq_change + (1 |studyid), data  = df_us)
ranova(mixed_full) 
summary(mixed_full_us)
performance::check_collinearity(mixed_full_us)
performance::check_heteroscedasticity(mixed_full_us)
performance::rmse(mixed_full_us)


PI <- predictInterval(merMod = mixed_full_mrm1, newdata = df,
					  level = 0.95, n.sims = 1000,
					  stat = "median", type="linear.prediction",
					  include.resid.var = TRUE)
##1. Mixed model with robust standard errors

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

#............Transfer Error
df_prediction <-  data.frame(predict(bayesian_mixed_full, df, allow_new_levels =TRUE)) 

transfer_error_fulldata <- df_prediction %>%
	tibble(lnwtp = df$lnwtp) %>%
	rename(lnwtp_pred = Estimate) %>%
	mutate(wtp_y = exp(lnwtp) - 1,
		   wtp_ypred = exp(lnwtp_pred) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))
mean(transfer_error_fulldata$TE_MA)
mean(transfer_error_fulldata$TE_BT)

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
	
	
	
	
	
	
	
	
	