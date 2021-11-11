
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
  mod_uscan = lmer(lnwtp ~ lnq0 + lnq_change + lnyear + local + us + lninc + forest + volunt + lumpsum + ce + nrev +
                     prov + reg + (1 |studyid), data= train)
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
sd(temp_full_all$RMSE_mr)
min(temp_full_all$RMSE_mr)
max(temp_full_all$RMSE_mr)
median(temp_full_all$RMSE_mr)

#b) Mean Value
mean(temp_full_all$RMSE_mv)
sd(temp_full_all$RMSE_mv)
min(temp_full_all$RMSE_mv)
max(temp_full_all$RMSE_mv)

median(temp_full_all$RMSE_mv)


#3. <<<<<<<<<<<<<<<<<<<<<<<Transfer Error --for the US only Model
#can dataset
df_can <- df %>% filter(us==0)
nrow(df_can)
meanlnwtp_can <- mean(df_can$lnwtp)
# predicting wetland values
canwtp_usmodel <- data.frame(wtp_fit=predict(Model_1c_us, newdata = df_can, allow.new.levels = T)) %>%
  mutate(lnwtp_pred = wtp_fit,
         lnwtp_or = df_can$lnwtp,
         meanlnwtpcan = meanlnwtp_can,
         error_mr = lnwtp_or - lnwtp_pred,
         RMSE_mr = sqrt((lnwtp_or - lnwtp_pred)^2),
         error_mv = meanlnwtpcan - lnwtp_pred,
         RMSE_mv = sqrt((meanlnwtpcan - lnwtp_pred)^2)
         )
canwtp_usmodel %>% View()
#transfer Error Summary Statistics for US-Canada combined data
#a) Meta-regression
mean(canwtp_usmodel$RMSE_mr)
sd(canwtp_usmodel$RMSE_mr)
min(canwtp_usmodel$RMSE_mr)
max(canwtp_usmodel$RMSE_mr)
median(canwtp_usmodel$RMSE_mr)

#b) Mean Value
mean(canwtp_usmodel$RMSE_mv) 
median(canwtp_usmodel$RMSE_mv)
sd(canwtp_usmodel$RMSE_mv) 
min(canwtp_usmodel$RMSE_mv)  
max(canwtp_usmodel$RMSE_mv)  
  

#a) Meta-regression



#b) Mean Value


