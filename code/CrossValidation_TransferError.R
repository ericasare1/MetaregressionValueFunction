
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

mvte_uscan_uscantestdata <- temp %>% gather(., RMSE, MAE ,key ="Metric",value = "Value") %>% 
  ggplot(aes(x=Metric,y=Value,fill=Metric)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~Metric, ncol=1, scales="free") +
  theme_bw() 
ggsave("output/mvte_uscan_uscantestdata.png")

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
ggsave("output/mrte_uscan_uscantestdata.png")

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
ggsave("output/mrte_uscan_cantestdata.png")


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
ggsave("output/mvte_uscan_cantestdata.png")

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
  test =temp_us[fold_us$subsets[fold_us$which == i], ]  # set first 1/1oth dataset for test
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
ggsave("output/mrte_us_cantestdata.png")


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
ggsave("output/mvte_us_cantestdata.png")

temp_us1 %>% dplyr::select(RMSE, MAE) %>% map_dbl(median,na.rm=T)

#library(plyr) : combine columns with different length
#combined <- rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])
#combined[25:40, ]

