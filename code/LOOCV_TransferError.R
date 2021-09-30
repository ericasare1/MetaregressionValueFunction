
#1. <<<<<<<<<<<<<<<<<<<<<<<<<<<<Transfer Error: US-Canada data are used as testing data <<<<<<<<<<<<<<<<<<<<
#Estimating the mean of lnwtp that will be used for the estimation of mean transfer error
df <- df %>% #whole data
  mutate(meanlnwtp = mean(lnwtp))
df_us <- df_us %>% #us data
  mutate(meanlnwtp = mean(lnwtp))

#a) Mean Value Transfer Error
set.seed(123) #set random seed for reproducibility of results

#splitting a data set to do 10 fold cv using the cvTools package
fold_cv = function(data,k){
  folds=cvTools::cvFolds(nrow(data),K= k)
  invisible(folds)
}

#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV
fold <- df %>% fold_cv(., k=nrow(df))
str(fold)

#creating a temp data to store results
temp_full <- df %>% 
  mutate(Fold=rep(0,nrow(df)),
         holdoutpred=rep(0,nrow(df)),
         MAD_mr=rep(0,nrow(.)),
         RMSE_mr=rep(0,nrow(.)),
         MAD_mv=rep(0,nrow(.)),
         RMSE_mv=rep(0,nrow(.)))

temp_full %>% View()
#train = temp_mrfull[fold$subsets[fold$which != i], ] %>% View()
for(i in 1:nrow(df)){
  train = temp_full[fold$subsets[fold$which != i], ]  #set the first n-1 dataset for training
  test = temp_full[fold$subsets[fold$which == i], ]  # set first 1/10th dataset for test
  mod_uscan = lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + us + prov + reg + 
                     cult + lninc + forest + volunt + lumpsum + ce + nrev +
                     (1 |studyid),  data= train)
  newpred <- data.frame(fit=predict(mod_uscan, newdata = test, allow.new.levels = T))
  true_mr = test$lnwtp #find the original true dependent var from testdata
  error_mr= (true_mr - newpred$fit) #deviations of true from predicted dependent variable
  
  true_mv = test$meanlnwtp #find the original true dependent var from testdata
  error_mv= (true_mv - newpred$fit) #deviations of true from predicted dependent variable
  
  #different measures of model fit
  rmse_mr=sqrt(error_mr^2)
  mae_mr=abs(error_mr)
  
  rmse_mv=sqrt(error_mv^2)
  mae_mv=abs(error_mv)
  
  #storing results from the cross validation looping
  temp_full[fold$subsets[fold$which == i], ]$holdoutpred <- newpred$fit
  temp_full[fold$subsets[fold$which == i], ]$RMSE_mr=rmse_mr
  temp_full[fold$subsets[fold$which == i], ]$MAD_mr=mae_mr
  temp_full[fold$subsets[fold$which == i], ]$RMSE_mv=rmse_mv
  temp_full[fold$subsets[fold$which == i], ]$MAD_mv=mae_mv
  temp_full[fold$subsets[fold$which == i], ]$Fold=i
}

temp_full_all <- temp_full %>% 
  dplyr::select(Fold, us, lnwtp, meanlnwtp, holdoutpred, RMSE_mr, MAD_mr, RMSE_mv, MAD_mv) %>% 
  filter(us == 0)
temp_full_all %>% View()

#transfer Error Summary Statistics for US-Canada combined data
#a) Meta-regression
mean(temp_full_all$RMSE_mr)

#b) Mean Value
mean(temp_full_all$RMSE_mv)


#3. <<<<<<<<<<<<<<<<<<<<<<<Transfer Error --for the US only Model
#can dataset
df_can <- df %>% filter(us==0)
df_can1 <- read_csv("data/df_can1.csv") #dina has been repeated 7 times just for testing
#df_dina[rep(1:nrow(df_dina), times = 7),]
nrow(df_can1)
nrow(df_us)
nrow(df_can)
#We apply the fold_cv function on our dataset… We set k=10 for a 10 folds CV
fold_us <- df_us %>% fold_cv(., k=nrow(df_us))
fold_cantest <- df_can1 %>% fold_cv(., k=nrow(df_can1))
#creating a temp data to store results
temp_full_us <- df_us %>% 
  mutate(Fold=rep(0,nrow(df_us)),
         holdoutpred=rep(0,nrow(df_us)),
         MAD_mr=rep(0,nrow(.)),
         RMSE_mr=rep(0,nrow(.)),
         MAD_mv=rep(0,nrow(.)),
         RMSE_mv=rep(0,nrow(.)),
         author = rep("",nrow(df_us)))
temp_full_us %>% View()
for(i in 1:nrow(df_us)){
  train = temp_full_us[fold_us$subsets[fold_us$which != i], ]  #set the first n-1 dataset for training
  test = df_can1[fold_cantest$subsets[fold_cantest$which == i], ]  # set first 1/1oth dataset for test
  mod_us = lmer(lnwtp ~ lnq0 + lnq_change + lnyear  + local + prov + reg + 
                  cult + lninc + forest + volunt + lumpsum + ce + nrev +
                  (1 |studyid),  data= train)
  newpred <- data.frame(fit=predict(mod_us, newdata = test, allow.new.levels = T))
  true_mr = test$lnwtp #find the original true dependent var from testdata
  error_mr= (true_mr - newpred$fit) #deviations of true from predicted dependent variable
  
  true_mv = test$meanlnwtp #find the original true dependent var from testdata
  error_mv= (true_mv - newpred$fit) #deviations of true from predicted dependent variable
  
  #different measures of model fit
  rmse_mr=sqrt(error_mr^2)
  mae_mr=abs(error_mr)
  
  rmse_mv=sqrt(error_mv^2)
  mae_mv=abs(error_mv)
  aut = test$authors
  
  #storing results from the cross validation looping
  temp_full_us[fold$subsets[fold$which == i], ]$holdoutpred <- newpred$fit
  temp_full_us[fold$subsets[fold$which == i], ]$RMSE_mr=rmse_mr
  temp_full_us[fold$subsets[fold$which == i], ]$MAD_mr=mae_mr
  temp_full_us[fold$subsets[fold$which == i], ]$RMSE_mv=rmse_mv
  temp_full_us[fold$subsets[fold$which == i], ]$MAD_mv=mae_mv
  temp_full_us[fold$subsets[fold$which == i], ]$Fold=i
  temp_full_us[fold$subsets[fold$which == i], ]$author=aut
  
}

temp_full_usall <- temp_full_us %>% 
  dplyr::select(authors, author, Fold, us, lnwtp, meanlnwtp, holdoutpred, RMSE_mr, MAD_mr, RMSE_mv, MAD_mv) 
temp_full_usall %>% View()

#transfer Error Summary Statistics for US-Canada combined data
#a) Meta-regression
mean(temp_full_usall$RMSE_mr)

#b) Mean Value
mean(temp_full_usall$RMSE_mv)  

#a) Meta-regression



#b) Mean Value


