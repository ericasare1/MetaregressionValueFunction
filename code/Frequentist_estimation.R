
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, merTools,  brms, rstanarm, rstan, sjstats, lmerTest)

# Import data
#-----------------------------------------------
df <- read_csv("data/Data_for_analysis_15_10.csv")

str(df) #check the structure of variables

#function to scale variables to be between 0 and 1
normalized <- function(x) {
	(x- min(x))/(max(x) - min(x))
}

# Creating variables
df <- df %>%
	filter(wlfresh == 1) %>%
	mutate(q0 = q0/1000,
		   q1 = q1/1000,
		   lnq0 = log(q0 * 1000),
		   lnq1 = log(q1 *1000),
		   q0_sc = normalized(q0),
		   q1_sc =  normalized(q1)) 

# Creating Change in baseline (q0) and post improvement wetland acres (q1) variables
df <- df %>% 
	mutate(q01 = (q1 + q0)/2,  
		   q_diff = q1- q0,
		   q_percent = ifelse(q0 == 0, 100, q1 / q0 -1), 
		   lnwtp2 = lnwtp - log(q1- q0),
		   us = ifelse(canada == 1, 0, 1), 
		   lnq_change = log(q1-q0),
		   canada_qo = q0 * canada,
		   canada_lnqo = lnq0 * canada)

# Variable Diagnostics
#A. checking the distribution of dependent variable
qqp(df$lnwtp, "norm")

#B. Plot of distribution of lnwtp within study clusters:  .
df$id_study <- as.character(df$studyid)     # create a character variable to identify obser. per study     
ggplot(df, aes(x= id_study, y = lnwtp, fill = as.character(us))) +
	geom_boxplot() +theme_bw()

#C. checking for outliers
boxplot(df$lnwtp) 

#a)....Checking for multicollinearity with Variance Inflation Factors
ols <- lm(lnwtp ~ lnyear  +
		 	local + 
		 	prov + reg + cult + lninc +
		 	forest + 
		 	volunt + lumpsum + ce + nrev + lnq_change + us, data  = df)
car::vif(ols) 

# Model Estimation
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

#. Random intercept model (MRM1 in Moeltner et al. 2019)
mixed_full <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
				   	volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us + (1 |studyid), data  = df)

#. Random intercept model (MRM2 in Moeltner et al. 2019)...
mixed_full <- lmer(lnwtp2 ~ lnyear  + local + prov + reg + cult + lninc +forest + 
				   	volunt + lumpsum + ce + nrev + q01 + us + (1 |studyid), data  = df)

#Model Diagnistics
summary(mixed_full)
fm <- step(mixed_full)
get_model(fm)
performance::icc(mixed_full) 
ranova(mixed_full) #checking if the random coefficient model is really significant
#checking for homo
plot(fitted(mixed_full), resid(mixed_full, type = "pearson"))# this will create the plot
abline(0,0, col="red")
#normality of residuals
qqnorm(resid(mixed_full)) 
qqline(resid(mixed_full), col = "red") # add a perfect fit line


library(merTools)

predictInterval(mixed_full)   # for various model predictions, possibly with new data

REsim(mixed_full)             # mean, median and sd of the random effect estimates

plotREsim(REsim(mixed_full))  # plot the interval estimates

tidy(mixed_full)
glance(mixed_full)

##1. Mixed model with robust standard errors

#bayesian

pr = prior(normal(0, 1), class = 'b') #assuming normal for the coefficients

bayesian_mixed_rest = brm(
	lnwtp ~ lnq_change + (1 | studyid), 
	data  = df,
	prior = pr,
	cores = 4 
)

summary(bayesian_mixed_rest)

#Full
bayesian_mixed_full = brm(
	lnwtp ~ lnyear  +
		local + 
		prov + reg + cult + lninc +
		forest + 
		volunt + lumpsum + ce + nrev + lnq_change + us + (1 | studyid), 
	data  = df,
	prior = pr,
	cores = 4,
	warmup = 500, 
	iter = 2000)
tidy(bayesian_mixed_full)

# examine random effects with the usual functions, not too tidy
# ranef(bayesian_mixed)
mixedup::extract_random_effects(bayesian_mixed_full)

#Here I plot the occasion effect, as well as the estimated predictions from the model vs. our observed GPA values.
conditional_effects(bayesian_mixed_full)


pp_check(bayesian_mixed_full)

#nonlin brm
priors = c(
	set_prior("normal(0,1)", class = "b", coef = "lnyear"),
	prior(normal(0,1), nlpar = gamma)
)

prior1 <- get_prior(
	bf(lnwtp ~ lnyear*b1 + log((exp(gamma * q1) - exp(gamma * q0)) / gamma),
	   b1 + gamma ~ 1, nl = TRUE),
	
	data = df,
	family = gaussian()
	
)

fit1 <- brm(
	bf(lnwtp ~ lnyear*b1 + log((exp(gamma * q1) - exp(gamma * q0)) / gamma),
	   b1 + gamma ~ 1, nl = TRUE),
	
	data = df,
	prior = prior1
	
)

#summary stats
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
stargazer(lm_restricted, lm_full,
		  type = "html",
		  out="ols.doc",
		  style = "qje",
		  single.row = TRUE)

class(mixed_restricted) <- "lmerMod"
class(mixed_full) <- "lmerMod"

stargazer(mixed_restricted, mixed_full,
		  type = "html",
		  out="mixed_models.doc",
		  style = "qje",
		  single.row = TRUE)

#nonlinear models
#. NLS

nlfunc <- function( ) {
	
	lnyear*b1  + local*b2 + prov*b3 + reg*b4 + cult*b5 + lninc*b6 + forest*b7 + 
	volunt*b8 + lumpsum*b9 + ce*b10 + nrev*b11 + lnq_change*b12 + 
		us*b13 + log((exp(b14 * q1) - exp(b14 * q0)) / b14)
	
}

ls_fit <- 








