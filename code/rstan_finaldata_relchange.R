# Setting Rstan data

df_freshwl <- df %>% filter(wlfresh ==1)
df_us_freshwl <- df_freshwl %>% filter(canada ==0)
df_can_freshwl <- df_freshwl %>% filter(canada ==1)
insample_pred <- data.frame(cbind(rep(1,nrow(as.matrix(df_can_freshwl[, 5:6]))), 
					   as.matrix(df_can_freshwl[, 5]),
					   as.matrix(df_can_freshwl[, 10:14]),
					   as.matrix(df_can_freshwl[, 17:19]), as.matrix(df_can_freshwl[,22])))

write.csv(df_freshwl, "data/df_freshwl.csv")
write.csv(df_us_freshwl, "data/df_us_fresh.csv")
write.csv(df_can_freshwl, "data/df_can_fresh.csv")
write.csv(insample_pred, "data/insample_pred.csv")

df_freshwl <- read.csv("data/df_freshwl.csv")
df_us_freshwl <- read.csv("data/df_us_fresh.csv")
df_can_freshwl <- read.csv("data/df_can_fresh.csv")
insample_pred <- read.csv("data/insample_pred.csv")

# Freshwater US and Canada Combined Dataset
n_ind <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_relchange <- list(N=nrow(df_freshwl),
						S = n_ind,
						lwtp=df_freshwl$lnwtp,
						x=cbind(rep(1,nrow(as.matrix(df_freshwl[, 5:6]))), 
								as.matrix(df_freshwl[, 5]),
								as.matrix(df_freshwl[, 10:14]),
								as.matrix(df_freshwl[, 17:19]), as.matrix(df_freshwl[,22])), 
						constant = (rep(1,nrow(as.matrix(df_freshwl[, 5:6])))),
						q0 = df_freshwl$q0,
						q0new = df_can_freshwl$q0,
						q1 = df_freshwl$q1,
						q1new = df_can_freshwl$q1,
						q_percent = df_freshwl$q_percent,
						q_percentnew = df_can_freshwl$q_percent)


data_stan_freshwl_relchange$K <- ncol(data_stan_freshwl_relchange$x)
data_stan_freshwl_relchange$xnew <- insample_pred
data_stan_freshwl_relchange$Nnew <- nrow(insample_pred)

# Freshwater US and Canada Combined Weakly informative
n_ind <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_relchange_restricted <- list(N=nrow(df_freshwl),
									S = n_ind,
									lwtp=df_freshwl$lnwtp,
									x1=rep(1,nrow(as.matrix(df_freshwl[, 5:6]))), 
									x2=df_freshwl$lnyear, x3 = df_freshwl$local, x4 = df_freshwl$prov,
									x5=df_freshwl$reg, x6=df_freshwl$cult,x7=df_freshwl$forest, 
									x8=df_freshwl$volunt, x9=df_freshwl$lumpsum, x10 = df_freshwl$ce, x11=df_freshwl$canada,
									x1new=rep(1,nrow(as.matrix(df_can_freshwl[, 5:6]))), 
									x2new=df_can_freshwl$lnyear, x3new = df_can_freshwl$local, 
									x4new = df_can_freshwl$prov, x5new=df_can_freshwl$reg,x6new=df_can_freshwl$cult,
									x7new=df_can_freshwl$forest, x8new=df_can_freshwl$volunt, 
									x9new=df_can_freshwl$lumpsum, x10new=df_can_freshwl$ce, x11new=df_can_freshwl$canada,	
									q0 = df_freshwl$q0,
									q0new = df_can_freshwl$q0, 
									q1 = df_freshwl$q1,
									q1new = df_can_freshwl$q1,
									q_percent = df_freshwl$q_percent,
									q_percentnew = df_can_freshwl$q_percent)

data_stan_freshwl_relchange_restricted$Nnew <- nrow(insample_pred)

# Freshwater US
n_ind_us_freshwl <- n_distinct(df_us_freshwl$studyid)
data_stan_us_freshwl_relchange <- list(N=nrow(df_us_freshwl),
						  S = n_ind_us_freshwl,
						  lwtp=df_us_freshwl$lnwtp,
						  #				  x=as.matrix(df[,6]),
						  x=cbind(rep(1,nrow(as.matrix(df_us_freshwl[, 5:6]))),
						  		as.matrix(df_us_freshwl[, 5]),
						  		as.matrix(df_us_freshwl[, 10:14]), 
						  		as.matrix(df_us_freshwl[, 17:19])),
						  q0 = df_us_freshwl$q0,
						  q0new = df_can_freshwl$q0,
						  q1 = df_us_freshwl$q1,
						  q1new = df_can_freshwl$q1,
						  q_percent = df_us_freshwl$q_percent,
						  q_percentnew = df_can_freshwl$q_percent)

data_stan_us_freshwl_relchange$K <- ncol(data_stan_us_freshwl_relchange$x)
data_stan_us_freshwl_relchange$xnew <- insample_pred[,1:10]
data_stan_us_freshwl_relchange$Nnew <- nrow(insample_pred)

# Freshwater US Dataset Weakly informative priors
n_ind <- n_distinct(df_us_freshwl$studyid)
data_stan_us_freshwl_relchange_restricted <- list(N=nrow(df_us_freshwl),
									S = n_ind,
									lwtp=df_us_freshwl$lnwtp,
									x1=rep(1,nrow(as.matrix(df_us_freshwl[, 5:6]))), 
									x2=df_us_freshwl$lnyear, x3 = df_us_freshwl$local, x4 = df_us_freshwl$prov,
									x5=df_us_freshwl$reg, x6=df_us_freshwl$cult,x7=df_us_freshwl$forest, 
									x8=df_us_freshwl$volunt, x9=df_us_freshwl$lumpsum, x10=df_us_freshwl$ce,
									x1new=rep(1,nrow(as.matrix(df_can_freshwl[, 5:6]))), 
									x2new=df_can_freshwl$lnyear, x3new = df_can_freshwl$local, 
									x4new = df_can_freshwl$prov, x5new=df_can_freshwl$reg,x6new=df_can_freshwl$cult,
									x7new=df_can_freshwl$forest, x8new=df_can_freshwl$volunt, 
									x9new=df_can_freshwl$lumpsum, x10new=df_can_freshwl$ce,	
									q0 = df_us_freshwl$q0,
									q0new = df_can_freshwl$q0, 
									q1 = df_us_freshwl$q1,
									q1new = df_can_freshwl$q1,
									q_percent = df_us_freshwl$q_percent,
									q_percentnew = df_can_freshwl$q_percent)

data_stan_us_freshwl_relchange_restricted$Nnew <- nrow(insample_pred)


###..Out of Sample Prediction ----PHJV Landscapes in Saskatchewan.
phjv_pred <- read_csv("data/phjv_prediction_data.csv")  #PHVJ Wetland acres Restoration Data
phjv_q1q0 <- read_csv("data/phjv_acres.csv") 

phjv_q1q0 <- phjv_q1q0 %>% 
	mutate(q01new = (q1 + q0)/2,
		   q_percentnew = ifelse(q0 == 0, 100, q1 / q0 -1), 
		   q_changenew = q1 - q0)

n_ind <- n_distinct(df_freshwl$studyid)
data_stan_freshwl_relchange_sk <- list(N=nrow(df_freshwl),
									S = n_ind,
									lwtp=df_freshwl$lnwtp,
									x=cbind(rep(1,nrow(as.matrix(df_freshwl[, 5:6]))), 
											as.matrix(df_freshwl[, 5]),
											as.matrix(df_freshwl[, 10:14]),
											as.matrix(df_freshwl[, 17:19])), 
									constant = (rep(1,nrow(as.matrix(df_freshwl[, 5:6])))),
									q0 = df_freshwl$q0,
									q0new = phjv_q1q0$q0,
									q1 = df_freshwl$q1,
									q1new = phjv_q1q0$q1,
									q_percent = df_freshwl$q_percent,
									q_percentnew = phjv_q1q0$q_percentnew)

data_stan_freshwl_relchange_sk$K <- ncol(data_stan_freshwl_relchange_sk$x)
data_stan_freshwl_relchange_sk$xnew <- phjv_pred 
data_stan_freshwl_relchange_sk$Nnew <- nrow(phjv_pred )
