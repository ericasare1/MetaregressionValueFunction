
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

#function to scale variables to be between 0 and 1
normalized <- function(x) {
  (x- min(x))/(max(x) - min(x))
}

# Creating and transforming variables
df <- df %>%
  filter(wlfresh == 1) %>%
  mutate(lnq0 = log(q0+1), #plus 1 to prevent taking the log of zero
         lnq_change = log(q1-q0),
         lnwtp2 = lnwtp - lnq_change,
         us = ifelse(authors=="us", 1,0)
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
Model_1c <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
					 volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us +
					   (1 |studyid), data  = df)

Model_1b <- lmer(lnwtp ~  lnq0 + lnq_change + us + prov + reg + cult + volunt + lumpsum +
                  (1 |studyid), data  = df)

Model_1 <- lmer(lnwtp ~  lnq0 + lnq_change + 
                   (1 |studyid), data  = df)
summary(Model_1c)
ranova(Model_1c) 
performance::performance_aic(Model_1c)

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
Model_2c <- lmer(lnwtp2 ~ lnyear  + local + prov + reg + cult +forest + 
                  volunt + lumpsum + ce + nrev + lnq0 + us +
                  (1 |studyid), data  = df) #lninc dropped cos of multicollinearity

Model_2 <- lmer(lnwtp2 ~  lnq0 + 
                  (1 |studyid), data  = df) #lninc dropped cos of multicollinearity

Model_2b <- lmer(lnwtp2 ~  lnq0 + prov + reg + cult + volunt + lumpsum + us +
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
#lnq_change + us + lninc + prov + reg + cult + volunt + lumpsum +
#. Model 1
Model_1_us <- lmer(lnwtp ~ lnyear  + local + prov + reg + cult  + forest +
							volunt + lumpsum + ce + lnq0 + lnq_change + (1 |studyid),
							data  = df_us)
Model_1b_us <- lmer(lnwtp ~  lnq0 + lnq_change + (1 |studyid),
                   data  = df_us)

Model_1c_us <- lmer(lnwtp ~  lnq0 + lnq_change + lninc + prov + reg + cult + volunt + lumpsum + (1 |studyid),
                    data  = df_us)
ranova(Model_1c_us) 
summary(Model_1c_us)
performance::check_collinearity(Model_1_us)
performance::check_heteroscedasticity(Model_1_us)
performance::rmse(Model_1_us)
performance::r2(Model_1_us)

#. Model 2
Model_2_us <- lmer(lnwtp2 ~ lnyear  + local + prov + reg + cult  + forest +
                     volunt + lumpsum + ce + nrev + lnq0 + (1 |studyid),
                   data  = df_us)

ranova(Model_2_us) # mixed model not appropriate for the data: We model ordinary least squares

summary(Model_2_us)
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

performance::performance_aic(Model_1c_us_ols)
performance::performance_aic(Model_2c_us_ols)


#............Transfer Error
#a) Mean Value Transfer Error
set.seed(123) #set random seed for reproducibility of results
df <- df %>%
  mutate(meanlnwtp = mean(lnwtp))
df_us <- df_us %>%
  mutate(meanlnwtp = mean(lnwtp))

#splitting a data set to do 10 fold cv using the cvTools package
fold_cv = function(data,k){
  folds=cvTools::cvFolds(nrow(data),K=k)
  invisible(folds)
}


#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV
fold <- df %>% fold_cv(., k=10)
str(fold)
#creating a temp data to store results
temp <- df %>% 
  mutate(Fold=rep(0,nrow(df)),
         holdoutpred=rep(0,nrow(df)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train =temp[fold$subsets[fold$which != i], ]  #set the first n-1 dataset for training
  test =temp[fold$subsets[fold$which == i], ]  # set first 1/1oth dataset for test
  mod_uscan = lmer(lnwtp ~  lnq0 + lnq_change + us + prov + reg + cult + volunt + lumpsum +
                   (1 |studyid),  data= train)
  newpred <- data.frame((predictInterval(merMod = mod_uscan, newdata = test,
                              level = 0.95, n.sims = 1000,
                              stat = "median", type="linear.prediction",
                              include.resid.var = TRUE)))
  true = test$meanlnwtp #find the original true dependent var from testdata
  error= (true - newpred$fit) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mae=mean(abs(error))
  #storing results from the cross validation looping
  temp[fold$subsets[fold$which == i], ]$holdoutpred <- newpred$fit
  temp[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp[fold$subsets[fold$which == i], ]$MSE=mae
  temp[fold$subsets[fold$which == i], ]$Fold=i
}

temp <- temp %>% rename(MAE = MSE) %>% dplyr::select(RMSE, MAE)

temp %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)

#B. Meta-regression Transfer Error
temp_uscan <- df %>% 
  mutate(Fold=rep(0,nrow(df)),
         holdoutpred=rep(0,nrow(df)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train =temp_uscan[fold$subsets[fold$which != i], ]  #set the first n-1 dataset for training
  test =temp_uscan[fold$subsets[fold$which == i], ]  # set first 1/1oth dataset for test
  mod_uscan = lmer(lnwtp ~  lnq0 + lnq_change + us + prov + reg + cult + volunt + lumpsum +
                     (1 |studyid),  data= train)
  newpred <- data.frame((predictInterval(merMod = mod_uscan, newdata = test,
                                         level = 0.95, n.sims = 1000,
                                         stat = "median", type="linear.prediction",
                                         include.resid.var = TRUE)))
  true = test$lnwtp #find the original true dependent var from testdata
  error= (true - newpred$fit) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mae=mean(abs(error))
  #storing results from the cross validation looping
  temp_uscan[fold$subsets[fold$which == i], ]$holdoutpred <- newpred$fit
  temp_uscan[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp_uscan[fold$subsets[fold$which == i], ]$MSE=mae
  temp_uscan[fold$subsets[fold$which == i], ]$Fold=i
}

temp_uscan <- temp_uscan %>% rename(MAE = MSE) %>% dplyr::select(RMSE, MAE)

temp_uscan %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp_uscan %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<transfer error : 4 data Canada points are used as test data (not included in the model data)
#preparing data for cv
df_can <- df %>% filter(us==0)
set.seed(123)
# creating 90% of the can dataset
#df_can_10 <- sample_frac(df_can, 0.5)
random_sample_can <- createDataPartition(df_can$lnwtp,
                                         p = 0.10, list = FALSE)
# generating training dataset
test_can <- df_can[random_sample_can, ] %>%
  mutate(meanlnwtp = mean(lnwtp))
nrow(test_can)
# generating testing dataset
train_can <- df_can[-random_sample_can, ] %>%
  mutate(meanlnwtp = mean(lnwtp))
ncol(train_can)
ncol(df_us)

df1 <- rbind(train_can, df_us)

#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV

fold1 <- df1 %>% fold_cv(., k=10)

#creating a temp data to store results
temp_can <- df1 %>% 
  mutate(Fold=rep(0,nrow(df1)),
         holdoutpred=rep(0,nrow(df1)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train =temp_can[fold1$subsets[fold1$which != i], ]  #set the first n-1 dataset for training
  #test =temp_uscan[fold$subsets[fold$which == i], ]  # set first 1/1oth dataset for test
  test = test_can
  mod_uscan = lmer(lnwtp ~  lnq0 + lnq_change + us + prov + reg + cult + volunt + lumpsum +
                     (1 |studyid),  data= train)
  newpred <- data.frame((predictInterval(merMod = mod_uscan, newdata = test,
                                         level = 0.95, n.sims = 1000,
                                         stat = "median", type="linear.prediction",
                                         include.resid.var = TRUE)))
  true = test$lnwtp #find the original true dependent var from testdata
  error= (true - newpred$fit) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mae=mean(abs(error))
  #storing results from the cross validation looping
  temp_can[fold1$subsets[fold1$which == i], ]$RMSE=rmse
  temp_can[fold1$subsets[fold1$which == i], ]$MSE=mae
  temp_can[fold1$subsets[fold1$which == i], ]$Fold=i
}

temp_can <- temp_can %>% rename(MAE = MSE) %>% dplyr::select(RMSE, MAE)

temp_can %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp_can %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)


#mean value TE: using 4 canadian data points as test data
#creating a temp data to store results
temp_can1 <- df1 %>% 
  mutate(Fold=rep(0,nrow(df1)),
         holdoutpred=rep(0,nrow(df1)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train =temp_can1[fold1$subsets[fold1$which != i], ]  #set the first n-1 dataset for training
  #test =temp_uscan[fold$subsets[fold$which == i], ]  # set first 1/1oth dataset for test
  test = test_can
  mod_uscan = lmer(lnwtp ~  lnq0 + lnq_change + us + prov + reg + cult + volunt + lumpsum +
                     (1 |studyid),  data= train)
  newpred <- data.frame((predictInterval(merMod = mod_uscan, newdata = test,
                                         level = 0.95, n.sims = 1000,
                                         stat = "median", type="linear.prediction",
                                         include.resid.var = TRUE)))
  true = test$meanlnwtp #find the original true dependent var from testdata
  error= (true - newpred$fit) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mae=mean(abs(error))
  #storing results from the cross validation looping
  temp_can1[fold1$subsets[fold1$which == i], ]$RMSE=rmse
  temp_can1[fold1$subsets[fold1$which == i], ]$MSE=mae
  temp_can1[fold1$subsets[fold1$which == i], ]$Fold=i
}

temp_can1 <- temp_can1 %>% rename(MAE = MSE) %>% dplyr::select(RMSE, MAE)

temp_can1 %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp_can1 %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)


# Transfer error --only usa model and testing data is same 4 can observations as above
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<transfer error : 4 data Canada points are used as test data (not 

#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV

fold_us <- df_us %>% fold_cv(., k=10)

#creating a temp data to store results
temp_us <- df_us %>% 
  mutate(Fold=rep(0,nrow(df_us)),
         holdoutpred=rep(0,nrow(df_us)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train_us =temp_us[fold_us$subsets[fold_us$which != i], ]  #set the first n-1 dataset for training
  #test =temp_uscan[fold$subsets[fold$which == i], ]  # set first 1/1oth dataset for test
  test = test_can
  mod_uscan = lm(lnwtp ~  lnq0 + lnq_change + prov + reg + cult + volunt + lumpsum,
                   data= train_us)
  newpred <- data.frame(fit = predict(mod_uscan, test))
  true = data.frame(original = test$lnwtp) #find the original true dependent var from testdata
  error = (true$original - newpred$fit) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mae=mean(abs(error))
  #storing results from the cross validation looping
  temp_us[fold_us$subsets[fold_us$which == i], ]$RMSE=rmse
  temp_us[fold_us$subsets[fold_us$which == i], ]$MSE=mae
  temp_us[fold_us$subsets[fold_us$which == i], ]$Fold=i
}

temp_us <- temp_us %>% rename(MAE = MSE) %>% dplyr::select(RMSE, MAE)

temp_us %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp_us %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)


#mean value TE: using 4 canadian data points as test data
#creating a temp data to store results
temp_us1 <- df_us %>% 
  mutate(Fold=rep(0,nrow(df_us)),
         holdoutpred=rep(0,nrow(df_us)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)))

for(i in 1:10){
  train_us =temp_us1[fold_us$subsets[fold_us$which != i], ]  #set the first n-1 dataset for training
  test =test_can  # set first 1/1oth dataset for test
  mod_uscan = lm(lnwtp ~  lnq0 + lnq_change + prov + reg + cult + volunt + lumpsum,
                 data= train_us)
  newpred <- predict(mod_uscan, test)
  true = test$meanlnwtp #find the original true dependent var from testdata
  error = (true - newpred) #deviations of true from predicted dependent variable
  #different measures of model fit
  rmse=sqrt(mean(error^2)) 
  mae=mean(abs(error))
  #storing results from the cross validation looping
  temp_us1[fold_us$subsets[fold_us$which == i], ]$RMSE=rmse
  temp_us1[fold_us$subsets[fold_us$which == i], ]$MSE=mae
  temp_us1[fold_us$subsets[fold_us$which == i], ]$Fold=i
}

temp_us1 <- temp_us1 %>% rename(MAE = MSE) %>% dplyr::select(RMSE, MAE)

temp_us1 %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp_us1 %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)





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

print(model_cv)

#splitting a data set to do 1o0 fold cv
#gen some test data

fold_cv = function(data,k){
  folds=cvTools::cvFolds(nrow(df),K=k)
  invisible(folds)
}

#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV
library(tidyverse)
set.seed(123)

fold <- df %>% fold_cv(., k=10)
str(fold)

#Then we will create a FOR loop, as you already know, the for loop require an object that receives the results generated by the loop. Here is our object:
temp <- df %>% 
  mutate(Fold=rep(0,nrow(df)),
         holdoutpred=rep(0,nrow(df)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)),
         MAE=rep(0,nrow(.)),
         R2=rep(0,nrow(.)),
         AIC=rep(0,nrow(.)),
         BIC=rep(0,nrow(.)))

for(i in 1:10){
  train=temp[fold$subsets[fold$which != i], ]
  test=temp[fold$subsets[fold$which == i], ]
  mod_1 = lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
                 volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us +
                 (1 |studyid), data  = train)
  newpred = tibble((predictInterval(merMod = mod_1, newdata = test,
                             level = 0.95, n.sims = 1000,
                             stat = "median", type="linear.prediction",
                             include.resid.var = T)))
  #newpred=predict(mod_1, newdata=test, allow.new.levels = T)
  true=test$lnwtp
  error=(true-newpred$fit)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred$fit-true)^2)
  R2=1-(sum((true-newpred$fit)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp[fold$subsets[fold$which == i], ]$holdoutpred <- newpred$fit
  temp[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp[fold$subsets[fold$which == i], ]$MSE=mse
  temp[fold$subsets[fold$which == i], ]$MAE=mae
  temp[fold$subsets[fold$which == i], ]$R2=R2
  temp[fold$subsets[fold$which == i], ]$AIC=AIC(mod_1)
  temp[fold$subsets[fold$which == i], ]$BIC=BIC(mod_1)
  temp[fold$subsets[fold$which == i], ]$Fold=i
}
temp

temp %>% gather(.,MSE:BIC,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric,ncol=1,scales="free")+
  theme_bw()

temp %>% dplyr::select(RMSE, MSE, MAE, R2, AIC, BIC) %>% map_dbl(median,na.rm=T)

# mean value 
temp2 <- df %>% 
  mutate(Fold=rep(0,nrow(df)),
         holdoutpred=rep(0,nrow(df)),
         MSE=rep(0,nrow(.)),
         RMSE=rep(0,nrow(.)),
         MAE=rep(0,nrow(.)),
         R2=rep(0,nrow(.)),
         AIC=rep(0,nrow(.)),
         BIC=rep(0,nrow(.)))

for(i in 1:10){
  train=temp2[fold$subsets[fold$which != i], ]
  test=temp2[fold$subsets[fold$which == i], ]
  mod_11 = lmer(lnwtp ~ lnyear  + local + prov + reg + cult + lninc +forest + 
                 volunt + lumpsum + ce + nrev + lnq0 + lnq_change + us +
                 (1 |studyid), data  = train)
  newpred = tibble((predictInterval(merMod = mod_11, newdata = test,
                                    level = 0.95, n.sims = 1000,
                                    stat = "median", type="linear.prediction",
                                    include.resid.var = T)))
  #newpred=predict(mod_1, newdata=test, allow.new.levels = T)
 # true=mean(test$lnwtp)
  true = rep(mean(test$lnwtp),nrow(test))
  error=(true-newpred$fit)
  rmse=sqrt(mean(error^2))
  mse=mean((newpred$fit-true)^2)
  R2=1-(sum((true-newpred$fit)^2)/sum((true-mean(true))^2))
  mae=mean(abs(error))
  temp2[fold$subsets[fold$which == i], ]$holdoutpred <- newpred$fit
  temp2[fold$subsets[fold$which == i], ]$RMSE=rmse
  temp2[fold$subsets[fold$which == i], ]$MSE=mse
  temp2[fold$subsets[fold$which == i], ]$MAE=mae
  temp2[fold$subsets[fold$which == i], ]$R2=R2
  temp2[fold$subsets[fold$which == i], ]$AIC=AIC(mod_1)
  temp2[fold$subsets[fold$which == i], ]$BIC=BIC(mod_1)
  temp2[fold$subsets[fold$which == i], ]$Fold=i
}
temp2

temp2 %>% gather(.,MSE:BIC,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw()

temp2 %>% dplyr::select(RMSE, MSE, MAE, R2, AIC, BIC) %>% map_dbl(median,na.rm=T)
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
