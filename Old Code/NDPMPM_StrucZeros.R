


###########################################################################
###########################################################################
####### MI using NDPMPM: Use rejection sampler to sample missing data #####
####### Add weighting option for sampling impossibles #####################
####### Add the Rejection sampler/sampling proposals hybrid option ########
####### Also make household head into a household level variable ##########
###########################################################################
###########################################################################

###################################### START ######################################
########################## Step 1: One Time Data Preparation ##########################
rm(list = ls())
set.seed(1010)
###### 1: Import Data
House <- read.csv("Data/House.csv",header=T)
Indiv <- read.csv("Data/Indiv.csv",header=T)


###### 2: Remove Households with size < 2 and > 4
House <- House[which(House$NP >= 2 & House$NP <= 4),]


###### 3: Keep only Households with TEN == 1,2,or 3 and recode 1,2 as 1 and 3 as 2
House <- House[which(House$TEN == 1 | House$TEN == 2 | House$TEN == 3),]
House$TEN[which(House$TEN == 2)] <- 1
House$TEN[which(House$TEN == 3)] <- 2


###### 4: Take a sample of size 5,000 Households
sample_size <- 5000
samp_index <- sort(sample(1:nrow(House),sample_size,replace=F))
House <- House[samp_index,]


###### 5: Pick the same households in the indiv data
pick_index <- is.element(Indiv$SERIALNO,House$SERIALNO)
Indiv <- Indiv[pick_index,]


###### 6: Recode within-household variables
###### 6a: First, the relationship variable
Indiv$RELP[which(Indiv$RELP == 11 | Indiv$RELP == 12 | Indiv$RELP == 13)] <- 12 #Boarder, roommate or partner
Indiv$RELP[which(Indiv$RELP == 14 | Indiv$RELP == 15)] <- 13 #Other non-relative or foster child
Indiv$RELP[which(Indiv$RELP == 10)] <- 11 #Other relative
Indiv$RELP[which(Indiv$RELP == 9)] <- 10 #Child-in-law
Indiv$RELP[which(Indiv$RELP == 8)] <- 9 #Parent-in-law
Indiv$RELP[which(Indiv$RELP == 7)] <- 8 #Grandchild
Indiv$RELP[which(Indiv$RELP == 6)] <- 7 #Parent
Indiv$RELP[which(Indiv$RELP == 5)] <- 6 #Sibling
Indiv$RELP[which(Indiv$RELP == 4)] <- 5 #Stepchild
Indiv$RELP[which(Indiv$RELP == 3)] <- 4 #Adopted child
Indiv$RELP[which(Indiv$RELP == 2)] <- 3 #Biological child
Indiv$RELP[which(Indiv$RELP == 1)] <- 2 #Spouse
Indiv$RELP[which(Indiv$RELP == 0)] <- 1 #Household head

###### 6b: Next, the race variable
Indiv$RAC3P[which(Indiv$RAC3P == 4 | Indiv$RAC3P == 8| Indiv$RAC3P == 9 | Indiv$RAC3P == 10)] <- 6
Indiv$RAC3P[which(Indiv$RAC3P == 5)] <- 4
Indiv$RAC3P[which(Indiv$RAC3P == 7)] <- 5
Indiv$RAC3P[which(Indiv$RAC3P >= 11 & Indiv$RAC3P <= 15)] <- 7
Indiv$RAC3P[which(Indiv$RAC3P >= 16 & Indiv$RAC3P <= 59)] <- 8
Indiv$RAC3P[which(Indiv$RAC3P >= 60 & Indiv$RAC3P <= 100)] <- 9

###### 6c: Next, the hisp variable
Indiv$HISP[which(Indiv$HISP >= 5 & Indiv$HISP <= 24)] <- 5

###### 6d: Lastly, age
Indiv$AGEP <- Indiv$AGEP + 1L


###### 7: Make household head into household level data
HHhead_data <- Indiv[which(Indiv$SPORDER==1),]
Indiv_minHH <- Indiv[-which(Indiv$SPORDER==1),]


###### 8: Combine Household and within-household data using the following ordering:
origdata <- data.frame(HHIndex = rep(c(1:sample_size),(House$NP-1L)),
                       WithinHHIndex = Indiv_minHH$SPORDER,
                       Gender = Indiv_minHH$SEX,Race = Indiv_minHH$RAC3P,Hisp = Indiv_minHH$HISP,
                       Age = Indiv_minHH$AGEP,Relate = Indiv_minHH$RELP,
                       Owner = rep(House$TEN,(House$NP-1L)),
                       HHGender = rep(HHhead_data$SEX,(House$NP-1L)),
                       HHRace = rep(HHhead_data$RAC3P,(House$NP-1L)),
                       HHHisp = rep(HHhead_data$HISP,(House$NP-1L)),
                       HHAge = rep(HHhead_data$AGEP,(House$NP-1L)),
                       HHRelate = rep(HHhead_data$RELP,(House$NP-1L)))


###### 9: Separate household and individual data
n_all <- length(unique(origdata$HHIndex))
X_indiv <- NULL
X_house <- NULL
for(i in 1:n_all){
  which_indiv <- which(origdata$HHIndex==i)
  X_indiv <- rbind(X_indiv,origdata[which_indiv,c("Gender","Race","Hisp","Age","Relate")])
  X_house <- rbind(X_house,cbind(length(which_indiv),origdata[
    which_indiv[length(which_indiv)],c("Owner","HHGender","HHRace","HHHisp","HHAge","HHRelate")]))
}
colnames(X_house) <- c("HHSize","Owner","HHGender","HHRace","HHHisp","HHAge","HHRelate")
X_house <- as.data.frame(X_house)
Data_indiv_truth <- X_indiv; Data_house_truth <- X_house


###### 10: Poke holes in Data:: Ignore missing household level data for now
#set.seed(0000)
N <- nrow(X_indiv)
n <- nrow(X_house)
n_i <- as.numeric(as.character(X_house[,1]))
p <- ncol(X_indiv)
q <- ncol(X_house)
house_index <- rep(c(1:n),n_i)
miss_samp_index <- sort(sample(1:nrow(X_house),(0.2*nrow(X_house)),replace=F))
O_house <- matrix(1,ncol=q,nrow=n)
colnames(O_house) <- colnames(X_house)
quick_miss_index <- c("Owner","HHGender","HHRace","HHHisp","HHAge")
O_house[miss_samp_index,quick_miss_index] <- rbinom((length(miss_samp_index)*length(quick_miss_index)),1,0.5)
X_house[O_house==0] <- NA
O_indiv <- matrix(1,ncol=p,nrow=N)
colnames(O_indiv) <- colnames(X_indiv)
others_names <- c("Gender","Race","Hisp","Age","Relate")
miss_samp_index_indiv <- which(is.element(house_index,miss_samp_index)==TRUE)
O_indiv[miss_samp_index_indiv,others_names] <- rbinom((length(miss_samp_index_indiv)*length(others_names)),1,0.50)
X_indiv[O_indiv==0] <- NA


###### 11: Save!!!
#write.table(origdata,"Data/origdata.txt",row.names = FALSE)
write.table(X_house, file = "Data/X_house.txt",row.names = FALSE)
write.table(X_indiv, file = "Data/X_indiv.txt",row.names = FALSE)
write.table(Data_house_truth, file = "Results/Data_house_truth.txt",row.names = FALSE)
write.table(Data_indiv_truth, file = "Results/Data_indiv_truth.txt",row.names = FALSE)
########################## End of Step 1 ##########################


###########################################################################
###########################################################################
#############################***Blank Space***#############################
###########################################################################
###########################################################################
###########################################################################


########################## Step 2: Model Fitting ##########################
rm(list = ls())
###### 1: Load prepared data, functions and packages; make sure data is in the right format
library(DirichletReg)
library(matrixStats)
library(coda)
Rcpp::sourceCpp('CppFunctions/prGpost.cpp')
Rcpp::sourceCpp('CppFunctions/prMpost.cpp')
Rcpp::sourceCpp('CppFunctions/checkSZ.cpp')
Rcpp::sourceCpp('CppFunctions/prHH.cpp')
source("OtherFunctions/OtherFunctions.R")
source("OtherFunctions/NDPMPM_No_StrucZeros.R")
X_house = read.table("Data/X_house.txt",header=TRUE)
X_indiv = read.table("Data/X_indiv.txt",header=TRUE)

level_indiv = list(c(1:2),c(1:9),c(1:5),c(1:96),c(2:13))
level_house = list(c(1:3),c(1:2),c(1:2),c(1:9),c(1:5),c(16:96),c(1))
Data_house <- data.frame(X_house)
for(i in 1:ncol(Data_house)){
  Data_house[,i] = factor(Data_house[,i],levels=level_house[[i]])
}
Data_indiv <- data.frame(X_indiv)
for(i in 1:ncol(Data_indiv)){
  Data_indiv[,i] = factor(Data_indiv[,i],levels=level_indiv[[i]])
}
#length(which(rowSums(is.na(Data_indiv[,c("Age","Relate")]))==2))/nrow(Data_indiv)
#About 8% individuals missing both age and relate...
#2% individuals missing age, relate and gender


###### 2: Set global parameters for data
N <- nrow(Data_indiv)
n <- nrow(Data_house)
n_i <- as.numeric(as.character(Data_house[,1]))
p <- ncol(Data_indiv)
q <- ncol(Data_house)
house_index <- rep(c(1:n),n_i)
n_i_index <- rep(n_i,n_i)


###### 3: Missing data indexes
NA_indiv <- Data_indiv; NA_house <- Data_house;
struc_zero_variables_indiv <- c(1,4,5)
nonstruc_zero_variables_indiv <- c(1:ncol(Data_indiv))[-struc_zero_variables_indiv]
struc_zero_variables_house <- c(1,4) + (q-p)
nonstruc_zero_variables_house <- c(1:ncol(Data_house))[-struc_zero_variables_house]
Indiv_miss_index_HH <- sort(unique(house_index[!complete.cases(NA_indiv[,struc_zero_variables_indiv])]))
n_miss <- length(Indiv_miss_index_HH)
Indiv_miss_index <- which(is.element(house_index,Indiv_miss_index_HH)==TRUE)
#n_i_miss <- n_i[Indiv_miss_index_HH]

###### 4a: Run unaugmented model with rejection sampler at the end and save proposals (one time only!!!),
###### Save only individuals data since age and gender are set to be always observed for household head
#proc_tt <- proc.time()
#n_prop <- 50; MM <- 50
#NDPMPM_proposals <- fit_NDPMPM(Data_house,Data_indiv,FF=30,SS=15,n_iter=10000,burn_in=5000,MM=MM,n_prop=n_prop,
#                               struc_zero=F,valid_prop=T,mc_thin=50,save_imp=F,save_prop=T)
#writeFun <- function(LL){names.ll <- names(LL);for(i in names.ll){
#  write.table(LL[[i]],paste0("Initial/",i,".txt"),row.names = FALSE)}}
#writeFun(NDPMPM_proposals)
#(proc.time() - proc_tt)[["elapsed"]]


###### 4b: Run unaugmented model with rejection sampler at every iteration and save imputation (one time only!!!)
###### This is just for model assessment!
#proc_tt <- proc.time()
#n_prop <- 50; MM <- 50
#NDPMPM_imput <- fit_NDPMPM(Data_house,Data_indiv,FF=30,SS=15,n_iter=10000,burn_in=5000,MM=MM,n_prop=n_prop,
#                           struc_zero=T,valid_prop=T,mc_thin=50,save_imp=T,save_prop=F)
#writeFun <- function(LL){names.ll <- names(LL);for(i in names.ll){
#  write.table(LL[[i]],paste0("Results/",i,".txt"),row.names = FALSE)}}
#writeFun(NDPMPM_imput)
#(proc.time() - proc_tt)[["elapsed"]]
#write.table((proc.time() - proc_tt)[["elapsed"]], file = "Results/total_time_nz.txt",row.names = FALSE)


###### 4c: Free some memory
#remove(NDPMPM_proposals)
#remove(NDPMPM_imput)

###### 5: Hybrid rejection
hybrid_option <- FALSE ### Remember to fix the hybrid piece of the MCMC.R code
n_prop <- 50
if(hybrid_option){
  ###### 5a: First fill missing values for household level and non-structural zeros variables
  if(sum(is.na(NA_house)) > 0){
    for (jj in 2:ncol(Data_house)){
      Data_house[is.na(Data_house[,jj]),jj] <-
        sample(level_house[[jj]],length(Data_house[is.na(Data_house[,jj]),jj]),replace=T,
               prob=summary(na.omit(Data_house[,jj])))
    }
  }

  if(sum(is.na(NA_indiv[nonstruc_zero_variables_indiv,])) > 0){
    for (ii in nonstruc_zero_variables_indiv){
      Data_indiv[is.na(Data_indiv[,ii]),ii] <-
        sample(level_indiv[[ii]],length(Data_indiv[is.na(Data_indiv[,ii]),ii]),replace=T,
               prob=summary(na.omit(Data_indiv[,ii])))
    }
  }

  ###### 5b: Load the posterior draws for individuals with the missing entries/data
  data_indiv_post <- read.table("Initial/DATA_INDIV_MISS.txt",header=TRUE)

  ###### 5c: Make the proposals into a list
  ###### Also, fill missing values with the proposal as a starting value
  Post_prop_indiv <- vector("list",n_miss)
  Post_prop_house_index <- rep(1:n_miss,n_i[Indiv_miss_index_HH])
  for(jj in 1:n_miss){
    another_index <- which(Post_prop_house_index == jj)
    for(kk in 1:n_prop){
      Post_prop_indiv[[jj]] <-
        rbind(Post_prop_indiv[[jj]],data_indiv_post[(another_index + (kk-1)*length(Indiv_miss_index)),])
      if(kk == 1){
        Data_indiv[which(is.element(house_index,Indiv_miss_index_HH[jj])==TRUE),] <-
          data_indiv_post[(another_index + (kk-1)*length(Indiv_miss_index)),]
      }
    }
  }
  hybrid_prob <- c(0.2,0.8) #first probability is for rejection sampler

} else {
  hybrid_prob <- c(1,0) #first probability is for rejection sampler
  ###### 5d: Fill missing values with starting values
  if(sum(is.na(NA_indiv)) > 0){
    for (ii in 1:ncol(Data_indiv)){
      Data_indiv[is.na(Data_indiv[,ii]),ii] <-
        sample(level_indiv[[ii]],length(Data_indiv[is.na(Data_indiv[,ii]),ii]),replace=T,
               prob=summary(na.omit(Data_indiv[,ii])))
    }
  }
  if(sum(is.na(NA_house)) > 0){
    for (jj in 2:ncol(Data_house)){
      Data_house[is.na(Data_house[,jj]),jj] <-
        sample(level_house[[jj]],length(Data_house[is.na(Data_house[,jj]),jj]),replace=T,
               prob=summary(na.omit(Data_house[,jj])))
    }
  }
}


###### 6: Calculate observed proportions and number of categories for each variable
d_k_house <- d_k_indiv <- ini_marg_house <- ini_marg_indiv <- NULL
for(k in 1:q){
  d_k_house <- cbind(d_k_house,nlevels(Data_house[,k]))
  ini_marg_k <- as.data.frame(table(Data_house[,k]))$Freq/sum(table(Data_house[,k]))
  ini_marg_k <- matrix(ini_marg_k,ncol=1)
  ini_marg_house <- rbind(ini_marg_house,ini_marg_k)  }
for(k in 1:p){
  d_k_indiv <- cbind(d_k_indiv,nlevels(Data_indiv[,k]))
  ini_marg_k <- as.data.frame(table(Data_indiv[,k]))$Freq/sum(table(Data_indiv[,k]))
  ini_marg_k <- matrix(ini_marg_k,ncol=1)
  ini_marg_indiv <- rbind(ini_marg_indiv,ini_marg_k)  }


###### 7: Set parameters for structural zeros
n_batch_init <- 1000 #sample impossibles in batches before checking constraints
n_0 <- rep(0,length(level_house[[1]]))
n_batch_imp_init <- 20 #sample imputations in batches before checking constraints
n_0_reject <- rep(0,n_miss)
prop_batch <- 1.2


###### 8: Weighting
weight_option <- TRUE #set to true for weighting/capping option
if(weight_option){
  struc_weight <- c(1/2,1/2,1/3) #set weights: must be ordered & no household size must be excluded
} else {
  struc_weight <- rep(1,length(level_house[[1]])) #set weights: must be ordered & no household size must be excluded
}
struc_weight <- as.matrix(struc_weight)
rownames(struc_weight) <- as.character(unique(sort(n_i)))


###### 9: Initialize chain
FF <- 30
SS <- 15
alpha <- beta <- 1
a_kdk <- 1
a_alpha <- b_alpha <- a_beta <- b_beta <- 0.25
lambda <- matrix(rep(ini_marg_house,FF),ncol=FF)
phi <- matrix(0,nrow=length(ini_marg_indiv),ncol=FF*SS) #make phi matrix and not array for c++
for(gm in 1:(FF*SS)){
    phi[,gm] <- ini_marg_indiv   }
U <- matrix(rbeta(FF,1,alpha),nrow=FF)
V <- matrix(rbeta((FF*SS),1,beta),nrow=FF,ncol=SS)
U[FF]<-1
V[,SS]<-1
one_min_U <- 1L-U
one_min_U <- c(1,cumprod(one_min_U[1:(FF-1)]))
one_min_V <- 1L-V
one_min_V <- cbind(1,t(apply(one_min_V[,-SS],1,cumprod)))
pii <- U*one_min_U
omega <- V*one_min_V
n_iter <- 10000
burn_in <- 0.5*n_iter
MM <- 50
mc_thin <- 1
M_to_use_mc <- sort(sample(seq((burn_in +1),n_iter,by=mc_thin),MM,replace=F))
d_k_indiv_cum <- 1+cumsum(c(0,d_k_indiv[,-p]))
d_k_house_cum <- 1+cumsum(c(0,d_k_house[,-q]))
FFF_indiv <- matrix(rep(cumsum(c(0,d_k_indiv[,-p])),each=N),ncol=p)
FFF_house <- matrix(rep(cumsum(c(0,d_k_house[,-q])),each=n),ncol=q)

###### 10: Create empty matrices to save results
dp_imput_house <- dp_imput_indiv <- NULL
ALPHA <- BETA <- PII <- G_CLUST <- M_CLUST <- N_ZERO <- NULL
#LAMBDA <- matrix(0,ncol=(ncol(lambda)*nrow(lambda)),nrow=(n_iter-burn_in))
#OMEGA <- matrix(0,ncol=(ncol(omega)*nrow(omega)),nrow=(n_iter-burn_in))
conv_check <- NULL

###### 11: Run MCMC
proc_total <- proc.time()
source("MCMC.R")
total_time <- (proc.time() - proc_total)[["elapsed"]]


###### 12: Save Results
if(hybrid_option){
  if(weight_option){
    MCMC_Results <- list(total_time_weighted_hybrid=total_time,
                         dp_imput_indiv_weighted_hybrid=dp_imput_indiv,
                         dp_imput_house_weighted_hybrid=dp_imput_house,
                         ALPHA_weighted_hybrid=ALPHA,BETA_weighted_hybrid=BETA,
                         N_ZERO_weighted_hybrid=N_ZERO,
                         M_CLUST_weighted_hybrid=M_CLUST,G_CLUST_weighted_hybrid=G_CLUST)
  } else {
    MCMC_Results <- list(total_time_hybrid=total_time,
                         dp_imput_indiv_hybrid=dp_imput_indiv,
                         dp_imput_house_hybrid=dp_imput_house,
                         ALPHA_hybrid=ALPHA,BETA_hybrid=BETA,N_ZERO_hybrid=N_ZERO,
                         M_CLUST_hybrid=M_CLUST,G_CLUST_hybrid=G_CLUST)
  }
} else {
  if(weight_option){
    MCMC_Results <- list(total_time_weighted=total_time,
                         dp_imput_indiv_weighted=dp_imput_indiv,
                         dp_imput_house_weighted=dp_imput_house,
                         ALPHA_weighted=ALPHA,BETA_weighted=BETA,N_ZERO_weighted=N_ZERO,
                         M_CLUST_weighted=M_CLUST,G_CLUST_weighted=G_CLUST)
  } else {
    MCMC_Results <- list(total_time=total_time,dp_imput_indiv=dp_imput_indiv,
                         dp_imput_house=dp_imput_house,
                         ALPHA=ALPHA,BETA=BETA,N_ZERO=N_ZERO,
                         M_CLUST=M_CLUST,G_CLUST=G_CLUST)
  }
}
writeFun <- function(LL){names.ll <- names(LL);for(i in names.ll){
    write.table(LL[[i]],paste0("Results/",i,".txt"),row.names = FALSE)}}
writeFun(MCMC_Results)
########################## End of Step 2 ##########################

###########################################################################
###########################################################################
#############################***Blank Space***#############################
###########################################################################
###########################################################################
###########################################################################


