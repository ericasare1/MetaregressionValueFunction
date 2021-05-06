
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,  brms, rstanarm, rstan, sjstats, lmerTest, caret)

# Import data
#-----------------------------------------------
df <- read_csv("data/Data_for_analysis_11_4_21.csv")
str(df) #check the structure of variables
df %>% View()
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
df_us <- df %>% filter(us == 1) %>% nrow() # number of observation of us studies
df_can <- df %>% filter(us == 0) %>% nrow() # number of observation of canadian studies

#dataframe to create correlation map from more relevant model variables
df_cor <- df %>%  
  dplyr::select(q0, q1, lnyear, local, prov, reg, cult, lninc, forest, 
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
cormat <- round(cor(as.matrix(df_cor),2))
highlyCorrelated <- findCorrelation(cor(as.matrix(df_cor)), cutoff=0.7, verbose = FALSE, names = T)
cormat <- cormat %>% filter(index == 19)
#highlyCorrelated_pro <- findCorrelation(cor(as.matrix(df_cor_prov)), cutoff= 0.7, verbose = FALSE, names = T)
print(highlyCorrelated)
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
Model_1c <- lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + us + prov + reg + 
                   cult + lninc + forest + volunt + lumpsum + ce + nrev +
					       (1 |studyid), data  = df)

Model_1b <- lmer(lnwtp ~  lnq0 + lnq_change + us + prov + reg + cult + volunt + lumpsum +
                  (1 |studyid), data  = df)

Model_1 <- lmer(lnwtp ~  lnq0 + lnq_change + 
                   (1 |studyid), data  = df)
summary(Model_1b)
ranova(Model_1) 
performance::performance_aic(Model_1c)
performance::r2(Model_1)

#US-Canada model summary results
class(Model_1) <- "lmerMod"
class(Model_1b) <- "lmerMod"
class(Model_1c) <- "lmerMod"


stargazer(Model_1, Model_1b, Model_1c,
          type = "html",
          out="output/model1-Us-Canada_models.doc",
          style = "qje",
          single.row = TRUE)

#Model 2: dep var is lnwtp2 and rel ind vars: lnqo
Model_2c <- lmer(lnwtp2 ~ lnq0 + lnyear + local + us + prov + reg + cult + forest + 
                  volunt + lumpsum + ce + nrev +
                  (1 |studyid), data  = df) #lninc dropped cos of multicollinearity

Model_2 <- lmer(lnwtp2 ~  lnq0 + 
                  (1 |studyid), data  = df) #lninc dropped cos of multicollinearity

Model_2b <- lmer(lnwtp2 ~  lnq0 + us + prov + reg + cult + volunt + lumpsum + 
                   (1 |studyid), data  = df) #lninc dropped cos of multicollinearity
summary(Model_2b)
ranova(Model_2) 

performance::performance_aic(Model_2c)

#US-Canada model summary results
class(Model_2) <- "lmerMod"
class(Model_2b) <- "lmerMod"
class(Model_2c) <- "lmerMod"


stargazer(Model_2, Model_2b, Model_2c,
          type = "html",
          out="output/model2-Us-Canada_models.doc",
          style = "qje",
          single.row = TRUE)

#Model Diagnostics
#checking if the random coefficient model is really significant

#Inter Class Correlation
performance::performance_aic(Model_1c) 
performance::performance_aic(Model_2c) 

#----------------US study only models--------------------
df_us <- df %>% filter(us ==1)
#lnq_change + us + lninc + prov + reg + cult + volunt + lumpsum +
#. Model 1

Model_1_us <- lmer(lnwtp ~  lnq0 + lnq_change + (1 |studyid),
                   data  = df_us)

Model_1b_us <- lmer(lnwtp ~  lnq0 + lnq_change + lninc + prov + reg + cult + volunt + lumpsum +                        (1 |studyid), data  = df_us)

Model_1c_us <- lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + prov + reg + cult  + forest +
                     volunt + lumpsum + ce + (1 |studyid),
                   data  = df_us)
ranova(Model_1b_us) 
summary(Model_1c_us)
performance::check_collinearity(Model_1_us)
performance::check_heteroscedasticity(Model_1_us)
performance::rmse(Model_1_us)
performance::r2(Model_1_us)

#. Model 2
Model_2_us <- lmer(lnwtp2 ~  lnq0  + (1 |studyid),
                   data  = df_us)

Model_2b_us <- lmer(lnwtp2 ~  lnq0 + lninc + prov + reg + cult + volunt + lumpsum +                        (1 |studyid), data  = df_us)

Model_2c_us <- lmer(lnwtp2 ~ lnq0 + lnq_change + lnyear  + local + prov + reg + cult  + forest +
                      volunt + lumpsum + ce + (1 |studyid),
                    data  = df_us)

ranova(Model_2b_us) # mixed model not appropriate for the data: We model ordinary least squares

summary(Model_2b_us)
performance::check_collinearity(Model_2_us)
performance::check_heteroscedasticity(Model_2_us)
performance::icc(Model_1_us)
performance::r2(Model_2_us)

#OLS
#. Model 1
Model_1c_us_ols <- lm(lnwtp ~ lnyear  + local + prov + reg + cult  + forest + lninc +
                     volunt + lumpsum + ce + lnq0 + lnq_change,
                   data  = df_us)

Model_1_us_ols <- lm(lnwtp ~  lnq0 + lnq_change,
                    data  = df_us)

Model_1b_us_ols <- lm(lnwtp ~  lnq0 + lnq_change + prov + reg + cult + volunt + lumpsum,
                    data  = df_us)

summary(Model_1_us_ols)
summary(Model_1b_us_ols)
summary(Model_1c_us_ols)

performance::check_heteroscedasticity(Model_1c_us_ols)

stargazer(Model_1_us_ols, Model_1b_us_ols, Model_1c_us_ols,
          type = "html",
          out="output/model1-Us_models.doc",
          style = "qje",
          single.row = TRUE)

#. Model 2
Model_2c_us_ols <- lm(lnwtp2 ~ lnyear  + local + prov + reg + cult  + forest +
                     volunt + lumpsum + ce + nrev + lnq0,
                   data  = df_us)

Model_2_us_ols <- lm(lnwtp2 ~  lnq0,
                      data  = df_us)

Model_2b_us_ols <- lm(lnwtp2 ~  lnq0 + lninc + prov + reg + cult + volunt + lumpsum,
                      data  = df_us)
summary(Model_2_us_ols)
summary(Model_2b_us_ols)
summary(Model_2c_us_ols)

stargazer(Model_2_us_ols, Model_2b_us_ols, Model_2c_us_ols,
          type = "html",
          out="output/model2-Us_models.doc",
          style = "qje",
          single.row = TRUE)

performance::performance_aic(Model_1_us_ols)
performance::performance_aic(Model_2_us_ols)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<bayesian>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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


df_can %>% mutate(change = q1-q0) %>% dplyr::select(authors, wtp_2017, change) 
