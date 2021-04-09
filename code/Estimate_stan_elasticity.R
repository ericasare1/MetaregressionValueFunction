
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)}

# Load Packages
p_load(tidyverse, rstan, shinystan, bayesplot, bridgesampling, loo, MCMCvis)

# Import data
#-----------------------------------------------
df <- read_csv("data/Data_for_analysis_15_10.csv")
df %>% View()
summary(df)

df <- df %>%
	filter(wlfresh == 1) %>%
	mutate(q0 = q0/1000,
		   q1 = q1/1000) 

df <- df %>% 
	mutate(q01 = (q1 + q0)/2,
		   q_percent = ifelse(q0 == 0, 100, q1 / q0 -1), 
		   lnwtp2 = lnwtp - log(q1- q0),
		   q_change = q1 - q0,
		   us = ifelse(canada == 1, 0, 1))
df %>% View()
#Summary statistics for key variables for US and Canada 
df %>%
	group_by(canada) %>%
	summarise(count = n(),
			  wtp = mean(exp(lnwtp)-1),
			  acre = mean(q1-q0)*1000,
			  wtp_acre = wtp/acre)

#Graph of lnacres vrs lnwtp for freshwater wetlands
lnacres_lnwtp <- df %>%
	mutate(lnacres = log((q1-q0)*1000)) %>%
	ggplot(aes(x=lnacres, y = lnwtp, label=canada)) +
	geom_point() +
	geom_text(aes(label=canada),hjust=0, vjust=0) + theme_bw() +
	labs(x = "Log(Acres)", y = "log(WTP)")

ggsave("output/graphs/logacres_vrs_logwtp_freshwater.png")

#Bayesian models
#a)....Checking for multicollinearity with Variance Inflation Factors
library(car)
lm1 <- lm(lnwtp ~  lnyear + lninc + 
		  	local + 
		  	prov + reg + cult + 
		  	forest + #q0 + q1 +
		  	volunt + lumpsum + ce + lnq_change, data  = df1)
car::vif(lm3) 
#b) ...Outliers...
boxplot(df$lnwtp)

df1 <-  df %>% mutate(lnq_change = log((q1+q0)/2))
lm2 <- lm(lnwtp ~  lnq_change, data  = df1)
lm3 <- lm(lnwtp ~ q01 + lnyear + us + 
		  	local + 
		  	prov + reg + cult + 
		  	forest + #q0 + q1 +
		  	volunt + lumpsum + ce + nrev + lnq_change, data  = df1)
cor(df1$lnwtp,df1$q_change)
#...............Setting up Bayesian Model estimation environment
# Set seed
seed = "12345"
# Choose number of iterations for models
n_iter <- 10000
n_chains <- 4

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#source('EstimateDiscountRate/code/stan/stan_utility.R')
# Set data list
#--------------------------------------------------------------------
init <- list(gamma = 0.08,
			 #	 beta = c(-.5, 0, .2, -0.4, -0.7, 3.1, -2.2, 1.6, -.3, 1.1, -0.02, 1.5),
			 sigma = .5)

init <- list(init = init,  
			 init = init,
			 init = init,
			 init = init)

# Linear model (M3c from Moeltner paper)
#relative change

ma_linear_lgq0_q1 <- stan("code/nonlinearMA_bridgesampling_lq1_q0.stan", 
							pars = c("beta", "sigma", "alpha"), init = init,
							data=data_stan_freshwl_relchange, iter= n_iter, chains= n_chains,
							control = list(adapt_delta = 0.99, max_treedepth = 15))
results_q1q0_constant  <- MCMCsummary(ma_linear_lgq0_q1, params = c("beta", "sigma", "alpha"), round = 6)
write_csv(results_q1q0_constant, "output/results/q1q0_constant.csv")

ma_linear_lgq0_q1_noconstant <- stan("code/nonlinearMA_bridgesampling_lq1_q0_noconstant.stan", 
						  pars = c("sigma", "alpha", "beta"), init = init,
						  data=data_stan_freshwl_relchange, iter= n_iter, chains= n_chains,
						  control = list(adapt_delta = 0.99, max_treedepth = 15))
results_q1q0_noconstant  <- MCMCsummary(ma_linear_lgq0_q1_noconstant, params = c("sigma", "alpha", "beta"), round = 6)
write_csv(results_q1q0_noconstant, "output/results/q1q0_nonconstant.csv")

ma_linear_relchange <- stan("code/linearMA_bridgesampling_relchange.stan", 
							pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),  init = init,
							data=data_stan_freshwl_relchange, iter = n_iter, chains= n_chains,
							control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear_relchange  <- MCMCsummary(ma_linear_relchange, params = c("beta", "sigma", "gamma"), round = 6)
write_csv(results_ma_linear_relchange, "output/results/ma_linear_relchange_22_10.csv")

ma_linear_relchange_restrict <- stan("code/linearMA_bridgesampling_relchange_restrict.stan", 
							pars = c("constant", "lnyear", "local", "pro", "reg", "cult","forest", 
									 "volump","lumpsum", "ce", "canada","sigma", "gamma", "y_rep", "log_lik"),
							init = init,
							data=data_stan_freshwl_relchange_restricted, iter = n_iter, chains= n_chains,
							control = list(adapt_delta = 0.99, max_treedepth =15))
results_ma_linear_relchange_restrict  <- MCMCsummary(ma_linear_relchange_restrict, params = c("constant", "lnyear", "local", "pro", "reg",  
																							  "cult","forest", "volump","lumpsum", "ce",    
																							  "canada","sigma", "gamma"), round = 6)
write_csv(results_ma_linear_relchange_restrict, "output/results/ma_linear_relchange_restrict_22_10.csv")

ma_linear_us_freshwl_relchange <- stan("code/linearMA_bridgesampling_relchange.stan", 
							 pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"), init = init,
							 data=data_stan_us_freshwl_relchange, iter= n_iter, chains= n_chains,
							 control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear_us_relchange  <- MCMCsummary(ma_linear_us_freshwl_relchange, params = c("beta", "sigma", "gamma"), round = 6)
write_csv(results_ma_linear_us_relchange, "output/results/ma_linear_us_fw_relchange_22_10.csv")

ma_linear_us_freshwl_relchange_restrict <- stan("code/linearMA_bridgesampling_relchange_restrict_us.stan", 
									   pars = c("constant", "lnyear", "local", "pro", "reg", "cult","forest", 
									   		 "volump","lumpsum", "ce","sigma", "gamma", "y_rep", "log_lik"), init = init,
									   data=data_stan_us_freshwl_relchange_restricted, iter= n_iter, chains= n_chains,
									   control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear_us_relchange_restrict  <- MCMCsummary(ma_linear_us_freshwl_relchange_restrict,
														params = c("constant", "lnyear", "local", "pro", "reg",  
																"cult","forest", "volump","lumpsum", "ce",    
																"sigma", "gamma"), round = 6)
write_csv(results_ma_linear_us_relchange_restrict, "output/results/ma_linear_us_fw_relchange_restrict_22_10.csv")

#not relative change
ma_linear <- stan("code/linearMA_bridgesampling.stan", 
						   pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"),  init = init,
						   data=data_stan_freshwl_relchange, iter= n_iter, chains= n_chains,
						   control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear  <- MCMCsummary(ma_linear, params = c("beta", "sigma", "gamma"), round = 6)
write_csv(results_ma_linear, "output/results/ma_linear_new_22_10.csv")

ma_linear_restrict <- stan("code/linearMA_bridgesampling_restrict.stan", 
							pars = c("constant","lnyear", "local", "pro", "reg", "cult","forest", "volump","lumpsum",
									 "ce", "canada", "sigma", "gamma", "log_lik", "y_rep"),  init = init,
							data=data_stan_freshwl_relchange_restricted, iter= n_iter, chains= n_chains,
							control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear_restrict  <- MCMCsummary(ma_linear_restrict, params = c("constant","lnyear", "local", "pro", "reg", "cult","forest", "volump","lumpsum", "ce", "canada", "sigma", "gamma"), round = 6)
write_csv(results_ma_linear_restrict, "output/results/ma_linear_restrict_22_10.csv")

ma_linear_us_freshwl <- stan("code/linearMA_bridgesampling.stan", 
									   pars = c("beta", "sigma", "gamma", "log_lik", "y_rep"), init = init,
									   data=data_stan_us_freshwl_relchange, iter= n_iter, chains= n_chains,
									   control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear_us  <- MCMCsummary(ma_linear_us_freshwl, params = c("beta", "sigma", "gamma"), round = 6)
write_csv(results_ma_linear_us, "output/results/ma_linear_us_new_22_10.csv")

ma_linear_us_freshwl_restrict <- stan("code/linearMA_bridgesampling_restrict_us.stan", 
									  pars = c("constant","lnyear", "local", "pro", "reg", "cult","forest", "volump","lumpsum", "ce", "sigma", "gamma", "log_lik", "y_rep"),  init = init,
									  data=data_stan_freshwl_relchange_restricted, iter= n_iter, chains= n_chains,
									  control = list(adapt_delta = 0.99, max_treedepth = 15))
results_ma_linear_us_restrict  <- MCMCsummary(ma_linear_us_freshwl_restrict, params = c("constant","lnyear", "local", "pro", "reg", "cult","forest", "volump","lumpsum","ce", "sigma", "gamma"), round = 6)
write_csv(results_ma_linear_us_restrict, "output/results/ma_linear_us_restrict_22_10.csv")

save(ma_linear_lgq0_q1, file="output/results/ma_linear_lgq1_q0.RData")
save(ma_linear_lgq0_q1_noconstant, file="output/results/ma_linear_q1q0noncosntant.RData")
save(ma_linear_relchange, file="output/results/ma_linear_relchange.RData")
save(ma_linear_relchange_restrict, file="output/results/ma_linear_relchange_restrict.RData")
save(ma_linear_us_freshwl_relchange, file="output/results/ma_linear_us_freshwl_relchange.RData")
save(ma_linear_us_freshwl_relchange_restrict, file="output/results/ma_linear_us_freshwl_relchange_restrict.RData")
save(ma_linear, file="output/results/ma_linearnew.RData")
save(ma_linear_restrict, file="output/results/ma_linear_restrict.RData")
save(ma_linear_us_freshwl, file="output/results/ma_linear_us_freshwlnew.RData")
save(ma_linear_us_freshwl_restrict, file="output/results/ma_linear_us_freshwl_restrict.RData")

#...................................Posterior Diagnostics....................
#A. Model Comparison: Bayes Factor, Marginal loglikelihood and RMSE
library(bridgesampling)
set.seed(1)

bridge_lin_wh_relchange <- bridge_sampler(ma_linear_relchange) 
bridge_lin_wh_relchange_restrict <- bridge_sampler(ma_linear_relchange_restrict) 
bridge_lin_wh <- bridge_sampler(ma_linear)
bridge_lin_wh_restrict <- bridge_sampler(ma_linear_restrict)


#compute Bayes factor
bf(bridge_lin_wh_relchange, bridge_lin_wh_relchange_restrict) #3027...restr winner
bf(bridge_lin_wh, bridge_lin_wh_restrict) # 406502_..restri

bf(bridge_lin_wh_relchange, bridge_lin_wh) #118.1...rec_restr
#Marginal loglikelihood
print(bridge_lin_wh_relchange)
print(bridge_lin_wh_relchange_restrict)

print(bridge_lin_wh)
print(bridge_lin_wh_restrict)

bridge_lin_wh_relchange_us <- bridge_sampler(ma_linear_us_freshwl_relchange)  
print(bridge_lin_wh_relchange_us)

#Relative Mean Square Errors
summary(bridge_lin_wh_relchange)
summary(bridge_lin_wh)

#...Probability parameter > 0
load("output/results/ma_linear_freshwl.RData")
load("output/results/ma_linear_freshwl_can.RData")

#Whole Freshwater Data (US and Canada)
mean(as.matrix(ma_linear_relchange)[, "beta[1]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[2]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[3]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[4]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[5]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[6]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[7]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[8]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[9]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[10]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "beta[11]"] > 0)
mean(as.matrix(ma_linear_relchange)[, "gamma"] > 0)
mean(as.matrix(ma_linear_relchange)[, "sigma"] > 0)


#US only model
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[1]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[2]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[3]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[4]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[5]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[6]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[7]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[8]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[9]"] > 0)
mean(as.matrix(ma_linear_us_freshwl_relchange)[, "beta[10]"] > 0)


#Post estimation diagnostics
#library(bayesplot)

#Autocorrelation 
ma_nonlinear_autocor <- stan_ac(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]', 'beta[11]'))
ma_nonlinear_us_autocor <- stan_ac(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										   'beta[9]', 'beta[10]'))
ma_nonlinear_cab_autocor <- stan_ac(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
									   'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
									   'beta[9]', 'beta[10]'))
#Trace
stan_trace(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										  'beta[9]', 'beta[10]', 'beta[11]', 'beta[12]'))
stan_trace(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]'))
stan_trace(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											  'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											  'beta[9]', 'beta[10]'))
# Effective Sample Size
stan_ess(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]', 'beta[11]'))
stan_ess(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											'beta[9]', 'beta[10]'))
stan_ess(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										'beta[9]', 'beta[10]', 'beta[11]'))
#Rhat
stan_rhat(ma_nonlinear, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
										 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
										 'beta[9]', 'beta[10]', 'beta[11]'))
stan_rhat(ma_nonlinear_us_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]'))
stan_rhat(ma_nonlinear_can_freshwl, pars = c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]',
											 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]',
											 'beta[9]', 'beta[10]'))
### Transfer Error; TE_MA = Meta-function Transfer Error, TE_BT = Benefit Transfer Error (predicted wtp = mean of original wtp)

fit_linear_fresh <- extract(ma_linear_relchange)
fit_linear_fresh_us <- extract(ma_linear_us_freshwl_relchange)

#extracting the predicted y (y_rep at the mean of posterior distribution)
y_prep_linfresh <- apply(fit_linear_fresh$y_rep, 2, mean) 
y_prep_linfresh_us <- apply(fit_linear_fresh_us$y_rep, 2, mean) 

# TE - linear _relchange
linfreshwater_TE <- data.frame(y_prep_linfresh, df_can_freshwl$lnwtp)
TransferErrors_lin <- linfreshwater_TE %>%	
		mutate(wtp_y = exp(df_can_freshwl.lnwtp) - 1,
		   wtp_ypred = exp(y_prep_linfresh) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))
median(TransferErrors_lin$TE_MA)
median(TransferErrors_lin$TE_BT)

#Distribution of TE_MA and TE_BT
boxplot(TransferErrors_lin$TE_MA)
boxplot(TransferErrors_lin$TE_BT)

write_csv(TransferErrors_lin, "output/results/TransferErrors_rechange.csv")

#linear Freshwater US Meta Function
linfreshwater_us_TE_relchnage <- data.frame(cbind(y_prep_linfresh_us, df_can_freshwl$lnwtp)) %>% rename(lnwtp_y = V2, lnwtp_pred = y_prep_linfresh_us)

TransferErrors_lin_us_relchg <- linfreshwater_us_TE_relchnage %>%
	mutate(wtp_y = exp(lnwtp_y) - 1,
		   wtp_ypred = exp(lnwtp_pred) - 1,
		   TE_MA = as.numeric((abs(wtp_y - wtp_ypred)/wtp_y)*100),
		   TE_BT = as.numeric((abs(wtp_y - mean(wtp_y))/wtp_y)*100))
median(TransferErrors_lin_us_relchg$TE_MA)
sd(TransferErrors_lin_us_relchg$TE_MA)
#Distribution of TE_MA and TE_BT
TransferErrors_lin_us_relchg %>%
	ggplot(aes(x = MA)) +
	geom_boxplot()
+
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "Alberta PHJV Landscape", y = "WTP (CAN$ 2018)")
boxplot(TransferErrors_lin_us_relchg$TE_MA)
ggsave("output/results/boxplot_us_MA.png")

boxplot(TransferErrors_lin_us_relchg$TE_BT)

write_csv(TransferErrors_lin_us_relchg, "output/results/TransferErrors_US_rechange.csv")

#b) Policy Application
#1) Saskachewan - PHJV Landscapes
phjv_locations <- read_csv("data/locations_new.csv")

ma_linear_relchange_sk <- stan("code/linearMA_bridgesampling_relchange.stan", 
							pars = c("y_rep"),  init = init,
							data=data_stan_freshwl_relchange_sk, iter = n_iter, chains= n_chains,
							control = list(adapt_delta = 0.99, max_treedepth = 15))

results_ma_linear_relchange_sk  <- MCMCsummary(ma_linear_relchange_sk, params = c("y_rep"), round = 6)
write_csv(results_ma_linear_relchange_sk, "output/results/ma_linear_relchange_sk.csv")

#extracting estimated parameters
fit_nonlinear_fresh_can_sk <- extract(ma_linear_relchange_sk)
y_prep_nonlinfresh_sk <- apply(fit_nonlinear_fresh_can_sk$y_rep, 2, mean) #extracting predicted lnwtp at means

linfreshwater_can_sk <- data.frame(y_prep_nonlinfresh_sk, phjv_locations) %>% 
	rename (lnwtp_pred = y_prep_nonlinfresh_sk, locations = PHJV.Target.Landscape,
			q1 = Estimated.Historical.Wetland.Area, q0 = Estimated.Remaining.Wetland.Area,
			province = Location) %>%
	mutate(wtp_ypred = exp(lnwtp_pred) - 1) %>%
	select(lnwtp_pred, locations, q1, q0, province, wtp_ypred, Estimated.Wetland.Area.Loss.2001.2011)


phjv_sask <-  linfreshwater_can_sk %>% filter(province == "S")
phjv_Alberta <-  linfreshwater_can_sk %>% filter(province == "A")
phjv_Manitoba <-  linfreshwater_can_sk %>% filter(province == "M")

write_csv(linfreshwater_can_sk, "output/results/phjv_wtp_predictions_new.csv")


#SK graoh
phjv_sask %>% 
	ggplot(aes(x= log(q1) , y = wtp_ypred)) +
	geom_point() +
	theme_bw() +
	labs(x = "Estimated Restoration Wetland Acres", y = "WTP (CAN$ 2018)")
ggsave("output/graphs/Restorationacres_vrs_wtp_pred_sk.png")

phjv_sask %>%
	ggplot(aes(x= locations, y = wtp_ypred)) +
	geom_bar(stat="identity") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "Saskatchewan PHJV Landscape", y = "WTP (CAN$ 2018)")
ggsave("output/graphs/wtppred_across_phvj_sk.png")

#AB graoh
mean(phjv_Alberta$wtp_ypred)
sd(phjv_Alberta$wtp_ypred)

phjv_Alberta %>% 
	ggplot(aes(x= log(q1) , y = wtp_ypred)) +
	geom_point() +
	theme_bw() +
	labs(x = "Estimated Restoration Wetland Acres", y = "WTP (CAN$ 2018)")
ggsave("output/graphs/Restorationacres_vrs_wtp_pred_AB.png")

phjv_Alberta %>%
	ggplot(aes(x= locations, y = wtp_ypred)) +
	geom_bar(stat="identity") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "Alberta PHJV Landscape", y = "WTP (CAN$ 2018)")
ggsave("output/graphs/wtppred_across_phvj_AB.png")

#M graoh
phjv_Manitoba %>% 
	ggplot(aes(x= log(q1) , y = wtp_ypred)) +
	geom_point() +
	theme_bw() +
	labs(x = "Estimated Restoration Wetland Acres", y = "WTP (CAN$ 2018)")
ggsave("output/graphs/Restorationacres_vrs_wtp_pred_MA.png")

phjv_Manitoba %>%
	ggplot(aes(x= locations, y = wtp_ypred)) +
	geom_bar(stat="identity") +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	labs(x = "Manitoba PHJV Landscape", y = "WTP (CAN$ 2018)")
ggsave("output/graphs/wtppred_across_phvj_MA.png")
