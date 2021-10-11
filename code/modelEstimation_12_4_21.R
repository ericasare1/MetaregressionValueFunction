
rm(list=ls(all=TRUE))

if (!require(pacman)) {
	install.packages("pacman")
	library(pacman)}

# Load Packages
p_load(sjPlot, tableone, stargazer, broom, tidyverse, lme4, car, MASS, WeMix, metafor, lmerTest, caret, gridExtra)

library(merTools)

# Import data
#-----------------------------------------------
df <- read_csv("data/Data_for_analysis_5_5_21.csv")
df <- df %>% dplyr::select(-c(X1, lnwtp)) %>% mutate(lnwtp = log(wtp_original))
df %>% View()
str(df) #check the structure of variables
df %>% dplyr::select(authors, wlfresh, q0, q1) %>% View()
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
df_us <- df %>% filter(us == 1) # number of observation of us studies
df_can <- df %>% filter(us == 0) # number of observation of canadian studies
++++++++++++++++++++++++nrow(df_us)
nrow(df_can)
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
cormat <- cor(as.matrix(df_cor))
#Reshape above matrix
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
#correlation heat map
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#<<<<<<<<<<<<<<<<<<Model Estimations <<<<<<<<<<<<
#Model 1: dep var is lnwtp and relevant independent variables are: lnqo and lnq_change

Model_1 <- lmer(lnwtp ~  lnq0 + lnq_change + 
                  (1 |studyid), data  = df)

Model_1b <- lmer(lnwtp ~ lnq0 + lnq_change + forest + volunt + lumpsum + ce + nrev  +
                   (1 |studyid), data  = df)

Model_1c <- lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + us + prov + reg + nrev +
                   cult + lninc + forest + volunt + lumpsum + ce  +
                   (1 |studyid), data  = df)

Model_1d <- lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + us + prov + reg + 
                   cult + lninc + forest + volunt + lumpsum + ce  +
					       (1 |studyid), data  = df)




#checking if the random intercenpt model is appropriate for the data
ranova(Model_1) 
ranova(Model_1b) 
ranova(Model_1c) 

#checking if the random intercenpt model is appropriate for the data
performance::check_collinearity(Model_1b)
performance::check_collinearity(Model_1c)
performance::check_collinearity(Model_1d)


summary(Model_1b)
summary(Model_1c)
summary(Model_1d)


# Calculating AIC
performance::performance_aic(Model_1b)
performance::performance_aic(Model_1c)
performance::performance_aic(Model_1d)


#Other post-estimation results
performance::check_heteroscedasticity(Model_1)
performance::check_heteroscedasticity(Model_1c)

performance::r2(Model_1b)
performance::r2(Model_1c)
performance::r2(Model_1d)


# Preparing to save model results in word
class(Model_1) <- "lmerMod"
class(Model_1b) <- "lmerMod"
class(Model_1c) <- "lmerMod"
class(Model_1d) <- "lmerMod"

stargazer(Model_1, Model_1b, Model_1c, Model_1d,
          type = "html",
          out="output/model1-Us-Canada_new.doc",
          style = "qje",
          single.row = TRUE)

#<<<<<<<Model 2: dependent variable is lnwtp2 and relevant independent vars: lnqo
Model_2 <- lmer(lnwtp2 ~  lnq0 + 
                  (1 |studyid), data  = df) #lninc dropped cos of multicollinearity
Model_2b <- lmer(lnwtp2 ~ lnq0  + forest + volunt + lumpsum + ce + nrev  +
                   (1 |studyid), data  = df)

Model_2c <- lmer(lnwtp2 ~ lnq0 + 
                   lnyear  + local + us + prov + reg + 
                   cult + lninc + forest + volunt + lumpsum + ce + nrev +
                   (1 |studyid), data  = df) #lninc dropped cos of multicollinearity

#checking if the random intercenpt model is appropriate for the data
ranova(Model_2)
ranova(Model_2b)
ranova(Model_2c) 
#checking if the random intercenpt model is appropriate for the data
performance::check_collinearity(Model_2b) #VIF all below 10 so all good
performance::check_collinearity(Model_2c) #VIF all below 10 so all good

# Calculating AIC
performance::performance_aic(Model_2b)
performance::performance_aic(Model_2c)

#Other post-estimation results
performance::check_heteroscedasticity(Model_2)
performance::check_heteroscedasticity(Model_2c)

#summary of resu1ts
summary(Model_2b)
summary(Model_2c)


performance::r2(Model_2c)
performance::r2(Model_2b)

#Saving results in word
class(Model_2) <- "lmerMod"
class(Model_2b) <- "lmerMod"
class(Model_2c) <- "lmerMod"


stargazer(Model_2, Model_2b, Model_2c,
          type = "html",
          out="output/model2-Us-Canada_new.doc",
          style = "qje",
          single.row = TRUE)

#---------------- --------------US study only models--------------------
df_us <- df %>% filter(us ==1)

#Model 1 as the one before but this time using the US only data
Model_1_us <- lmer(lnwtp ~  lnq0 + lnq_change + (1 |studyid),
                   data  = df_us)

Model_1b_us <- lmer(lnwtp ~  lnq0 + lnq_change + prov + reg + cult + lninc + forest + 
                       + volunt + lumpsum + (1 |studyid), data  = df_us)

Model_1c_us <- lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + prov + reg + 
                   cult + lninc + forest + volunt + lumpsum + ce + nrev +
                   (1 |studyid), data  = df_us)           
                      #lnyear  + local + prov + reg + 
                     # cult + lninc + 
summary(Model_1_us)
summary(Model_1b_us)
summary(Model_1c_us)



#Model_1c_us <- lmer(lnwtp ~  lnq0 + lnq_change + lnyear + local + prov + reg + cult + forest + lninc + lumpsum  + ce + nrev + (1 |studyid), data  = df_us)

#checking if the random intercenpt model is appropriate for the data
ranova(Model_1b_us) 
ranova(Model_1c_us) 
#checking if the random intercenpt model is appropriate for the data
performance::check_collinearity(Model_1c_us) #VIF all below 10 so all good
performance::check_collinearity(Model_1b_us) #VIF all below 10 so all good

# Calculating AIC
performance::performance_aic(Model_1b_us)
performance::performance_aic(Model_1c_us)

#Other post-estimation results
performance::check_heteroscedasticity(Model_1_us)
performance::check_heteroscedasticity(Model_1c_us)

#summary of resu1ts
summary(Model_1b_us)
summary(Model_1c_us)


performance::r2(Model_1b_us)
performance::r2(Model_1c_us)

#US-Canada model summary results
class(Model_1_us) <- "lmerMod"
class(Model_1b_us) <- "lmerMod"
class(Model_1c_us) <- "lmerMod"


stargazer(Model_1_us, Model_1b_us, Model_1c_us,
          type = "html",
          out="output/model1-Us-new.doc",
          style = "qje",
          single.row = TRUE)

#. <<<<<<<<<<<<<<<<<<<<<<<<<<<Model 2
##Model 2 as the one before but this time using the US only data
library(lme4)
library(lmerTest)
Model_2_us <- lmer(lnwtp2 ~  lnq0  + (1 |studyid),
                   data  = df_us)
Model_2b_us <- lmer(lnwtp2 ~  lnq0 + prov + reg + cult + lninc + forest + 
                      + volunt + lumpsum + (1 |studyid), data  = df_us)

Model_2c_us <- lmer(lnwtp2 ~  lnq0 + lnyear  + local + prov + reg + 
                      cult + lninc + forest + volunt + lumpsum + ce + nrev  + (1 |studyid), data  = df_us)

#Model_2c_us <- lmer(lnwtp2 ~ lnq0 + lnyear + local + prov + reg + cult + forest  + lumpsum  + ce + nrev + (1 |studyid),data  = df_us)


#checking if the random intercenpt model is appropriate for the data
ranova(Model_2b_us) 
ranova(Model_2c_us)
ranova(Model_2_us) 

#checking if the random intercenpt model is appropriate for the data
performance::check_collinearity(Model_2c_us) #VIF all below 10 so all good
performance::check_collinearity(Model_2b_us) #VIF all below 10 so all good

# Calculating AIC
performance::performance_aic(Model_2b_us)
performance::performance_aic(Model_2c_us)

#Other post-estimation results
performance::check_heteroscedasticity(Model_2_us)
performance::check_heteroscedasticity(Model_2c_us)

#summary of resu1ts
summary(Model_2b_us)
summary(Model_2c_us)

performance::r2(Model_2_us)
performance::r2(Model_2c_us)

#Preparing to save results in word
class(Model_2_us) <- "lmerMod"
class(Model_2b_us) <- "lmerMod"
class(Model_2c_us) <- "lmerMod"


stargazer(Model_2_us, Model_2b_us, Model_2c_us,
          type = "html",
          out="output/model2-Us-new.doc",
          style = "qje",
          single.row = TRUE)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< End >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>