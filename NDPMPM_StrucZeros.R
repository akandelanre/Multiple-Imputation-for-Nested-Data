


###########################################################################
###########################################################################
####### MI using NDPMPM: Use rejection sampler to sample missing data #####
####### Also make household head into a household level variable ##########
###########################################################################
###########################################################################

###################################### START ######################################
########################## Step 1: One Time Data Preparation ########################## 
rm(list = ls())
###### 1: Import Data
House <- read.csv("Data/House.csv",header=T)
Indiv <- read.csv("Data/Indiv.csv",header=T)

###### 2: Remove Households with size < 2 and > 4
House <- House[which(House$NP >= 2 & House$NP <= 4),]

###### 3: Keep only Households with TEN == 1,2,or 3 and recode 1,2 as 1 and 3 as 2
House <- House[which(House$TEN == 1 | House$TEN == 2 | House$TEN == 3),]
House$TEN[which(House$TEN == 2)] <- 1
House$TEN[which(House$TEN == 3)] <- 2

###### 4: Take a sample of size 2,000 Households
set.seed(419)
sample_size <- 2000
samp_index <- sort(sample(1:nrow(House),sample_size,replace=F))
House <- House[samp_index,]

###### 5: Pick the same households in the indiv data
pick_index <- is.element(Indiv$SERIALNO,House$SERIALNO)
Indiv <- Indiv[pick_index,]

###### 6: Recode within-household variables
###### 6a: First, the relationship variable
Indiv$RELP[which(Indiv$RELP == 12 | Indiv$RELP == 13)] <- 11
Indiv$RELP[which(Indiv$RELP == 14 | Indiv$RELP == 15)] <- 12
Indiv$RELP[which(Indiv$RELP == 2 | Indiv$RELP == 4)] <- 3
Indiv$RELP[which(Indiv$RELP == 1)] <- 2
Indiv$RELP[which(Indiv$RELP == 0)] <- 1
Indiv$RELP[which(Indiv$RELP == 9)] <- 4
Indiv$RELP[which(Indiv$RELP == 7)] <- 9
Indiv$RELP[which(Indiv$RELP == 5)] <- 7
Indiv$RELP[which(Indiv$RELP == 6)] <- 5
Indiv$RELP[which(Indiv$RELP == 8)] <- 6
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
###### c("HHIndex","WithinHHIndex","Gender","Race","Hisp","Age","Relate","Owner")
#origdata <- data.frame(HHIndex = rep(c(1:sample_size),House$NP),WithinHHIndex = Indiv$SPORDER,
#                       Gender = Indiv$SEX,Race = Indiv$RAC3P,Hisp = Indiv$HISP,
#                       Age = Indiv$AGEP,Relate = Indiv$RELP,Owner = rep(House$TEN,House$NP))
#colnames(origdata) <- c("HHIndex","WithinHHIndex","Gender","Race","Hisp","Age","Relate","Owner")

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

###### 8: Save!!!
write.table(origdata,"Data/origdata.txt",row.names = FALSE)

###### 9: Load the data back
origdata <- read.table("Data/origdata.txt",header=T)

###### 10: Separate household and individual data
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

###### 11: Save!!!
write.table(X_house, file = "Data/X_house.txt",row.names = FALSE)
write.table(X_indiv, file = "Data/X_indiv.txt",row.names = FALSE)
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
source("OtherFunctions/OtherFunctions.R")
source("OtherFunctions/NDPMPM_No_StrucZeros.R")
X_house = read.table("Data/X_house.txt",header=TRUE)
X_indiv = read.table("Data/X_indiv.txt",header=TRUE)
level_indiv = list(c(1:2),c(1:9),c(1:5),c(1:100),c(2:12))
level_house = list(c(1:3),c(1:2),c(1:2),c(1:9),c(1:5),c(1:100),c(1))
Data_house <- data.frame(X_house)
for(i in 1:ncol(Data_house)){
  Data_house[,i] = factor(Data_house[,i],levels=level_house[[i]])
}
Data_indiv <- data.frame(X_indiv)
for(i in 1:ncol(Data_indiv)){
  Data_indiv[,i] = factor(Data_indiv[,i],levels=level_indiv[[i]])
}
Data_indiv_truth <- Data_indiv; Data_house_truth <- Data_house


###### 2: Set global parameters for data
N <- nrow(Data_indiv)
n <- nrow(Data_house)
n_i <- as.numeric(as.character(Data_house[,1]))
p <- ncol(Data_indiv)
q <- ncol(Data_house)
house_index <- rep(c(1:n),n_i)
n_i_index <- rep(n_i,n_i)


###### 3: Poke holes in Data:: Ignore missing household level data for now 
### First make sure 75% of the households have complete data
### Also, make sure household head is always observed
### 40% have only relate missing, 40% have only age missing and 20% have both missing.
### The other individual level variables are missing with 40% probability
set.seed(419)
n_miss <- 0.35*n
House_miss_index <- NULL
Indiv_miss_index_HH <- sample(1:n,n_miss,replace=FALSE)
Indiv_miss_index <- which(is.element(house_index,Indiv_miss_index_HH)==TRUE) #already sorted
for(i in 1:n_miss){
  another_index <- which(is.element(house_index,Indiv_miss_index_HH[i])==TRUE)
  sub_sample <- another_index[sample(length(another_index),sample(length(another_index),1,replace=F),replace=F)]
  if(i <= (0.35*n_miss)){
    Data_indiv[sub_sample,"Age"] <- NA
  }
  if(i > (0.35*n_miss) & i <= (0.7*n_miss)){
    Data_indiv[sub_sample,"Relate"] <- NA
  }
  if(i > (0.7*n_miss)){
    Data_indiv[sub_sample,c("Age","Relate")] <- NA
  }
}
O_indiv <- matrix(1,ncol=p,nrow=N)
colnames(O_indiv) <- colnames(Data_indiv)
others_names <- c("Gender","Race")
O_indiv[Indiv_miss_index,others_names] <- rbinom((length(Indiv_miss_index)*length(others_names)),1,0.5)
Data_indiv[O_indiv==0] <- NA
NA_indiv <- Data_indiv; NA_house <- Data_house;
Indiv_miss_index_HH <- sort(Indiv_miss_index_HH)
Data_indiv_cc <- Data_indiv[-Indiv_miss_index,]
Data_house_cc <- Data_house[-Indiv_miss_index_HH,]


###### 4a: Run NDPMPM without structural zeros for comparison
#n_proposed <- 10;
#NDPMPM_results <- fit_NDPMPM(Data_house,Data_indiv,FF=20,SS=15,n_iter=10000,burn_in=8000,MM=n_proposed,struc_zero=TRUE)
#writeFun <- function(LL){names.ll <- names(LL);for(i in names.ll){
#  write.table(LL[[i]],paste0("Initial/",i,".txt"),row.names = FALSE)}}
#writeFun(NDPMPM_results)


###### 4b: Load the posterior draws for individuals with the missing entries/data
#data_house_post <- read.table("Initial/DATA_HOUSE_MISS.txt",header=TRUE)
#data_indiv_post <- read.table("Initial/DATA_INDIV_MISS.txt",header=TRUE)
dp_imput_indiv_nz <- read.table("Initial/dp_imput_indiv.txt",header=TRUE)
dp_imput_house_nz <- read.table("Initial/dp_imput_house.txt",header=TRUE)


###### 4: Fill missing values with starting values
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


###### 5: Calculate observed proportions and number of categories for each variable
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


###### 6: Set parameters for structural zeros
n_batch_init <- rep(100,length(level_house[[1]])) #sample impossibles in batches before checking constraints
n_0 <- rep(0,length(level_house[[1]]))
n_batch_imp_init <- rep(10,n_miss) #sample imputations in batches before checking constraints
n_0_reject <- rep(0,n_miss)
prop_batch <- 1.2


###### 7: Initialize chain
FF <- 20
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
burn_in <- 0.9*n_iter
MM <- 10
M_to_use <- seq((burn_in+1), n_iter, (length(c((burn_in+1):n_iter))/(5*MM)))
M_to_use_mc <- sample(M_to_use,MM,replace=FALSE)
d_k_indiv_cum <- 1+cumsum(c(0,d_k_indiv[,-p]))
d_k_house_cum <- 1+cumsum(c(0,d_k_house[,-q]))
FFF_indiv <- matrix(rep(cumsum(c(0,d_k_indiv[,-p])),each=N),ncol=p)
FFF_house <- matrix(rep(cumsum(c(0,d_k_house[,-q])),each=n),ncol=q)


###### 8: Create empty matrices to save results
dp_imput_house <- dp_imput_indiv <- NULL
ALPHA <- BETA <- PII <- G_CLUST <- M_CLUST <- N_ZERO <- NULL
#LAMBDA <- matrix(0,ncol=(ncol(lambda)*nrow(lambda)),nrow=(n_iter-burn_in))
#OMEGA <- matrix(0,ncol=(ncol(omega)*nrow(omega)),nrow=(n_iter-burn_in))


###### 9: Run MCMC
source("MCMC.R")
MCMC_Results <- list(Data_house_truth=Data_house_truth,Data_indiv_truth=Data_indiv_truth,
                     Data_house_cc=Data_house_cc,Data_indiv_cc=Data_indiv_cc,
                     dp_imput_indiv=dp_imput_indiv,dp_imput_house=dp_imput_house,
                     dp_imput_indiv_nz=dp_imput_indiv_nz,dp_imput_house_nz=dp_imput_house_nz,
                     ALPHA=ALPHA,BETA=BETA,N_ZERO=N_ZERO)

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


########################## Step 3: Model Assessment ##########################
rm(list = ls())
###### 1: Define parameters and load saved results
mm <- 10;
Data_house_truth <- read.table("Results/Data_house_truth.txt",header=TRUE)
Data_indiv_truth <- read.table("Results/Data_indiv_truth.txt",header=TRUE)
Data_house_cc <- read.table("Results/Data_house_cc.txt",header=TRUE)
Data_indiv_cc <- read.table("Results/Data_indiv_cc.txt",header=TRUE)
dp_imput_house <- read.table("Results/dp_imput_house.txt",header=TRUE)
dp_imput_indiv <- read.table("Results/dp_imput_indiv.txt",header=TRUE)
dp_imput_house_nz <- read.table("Results/dp_imput_house_nz.txt",header=TRUE)
dp_imput_indiv_nz <- read.table("Results/dp_imput_indiv_nz.txt",header=TRUE)
N <- nrow(Data_indiv_truth)
n <- nrow(Data_house_truth)
n_i <- as.numeric(as.character(Data_house_truth[,1]))
p <- ncol(Data_indiv_truth)
q <- ncol(Data_house_truth)
house_index <- rep(c(1:n),n_i)
N_cc <- nrow(Data_indiv_cc)
n_cc <- nrow(Data_house_cc)
n_i_cc <- as.numeric(as.character(Data_house_cc[,1]))
house_index_cc <- rep(c(1:n_cc),n_i_cc)


###### 2: Calculate probabilities that depend on relationship variable from original data
Probs <- matrix(0,nrow=22)
n_row_2 <- length(which(n_i==1))
n_row_3 <- length(which(n_i==2))
n_row_4 <- length(which(n_i==3))
samp_size <- matrix(c(n_row_2,n_row_3,n_row_4,rep(n,19)),22,1)
for(kk in 1:n){
  hh_check <- Data_house_truth[kk,(q-p+1):q]
  colnames(hh_check) <- colnames(Data_indiv_truth)
  hh_check <- rbind(hh_check,Data_indiv_truth[which(house_index==kk),])
  hh_check <- data.frame(Owner=Data_house_truth[kk,"Owner"],hh_check)
  if(nrow(hh_check)==2 && hh_check[1,"Race"]==hh_check[2,"Race"]){
    Probs[1] <- Probs[1] + 1 #All same race, n_i = 2
  }
  if(nrow(hh_check)==3 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
     hh_check[2,"Race"]==hh_check[3,"Race"]){
    Probs[2] <- Probs[2] + 1 #All same race, n_i = 3
  }
  if(nrow(hh_check)==4 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
     hh_check[2,"Race"]==hh_check[3,"Race"]&&hh_check[3,"Race"]==hh_check[4,"Race"]){
    Probs[3] <- Probs[3] + 1 #All same race, n_i = 4
  }
  if(sum(hh_check[,"Relate"]==2)>=1){
    Probs[4] <- Probs[4] + 1 #Spouse present
  }
  if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
    Probs[5] <- Probs[5] + 1 #Spouse with white HH
  }
  if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==2)>=1){
    Probs[6] <- Probs[6] + 1 #Spouse with black HH
  }
  if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
     sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
    Probs[7] <- Probs[7] + 1 #White Couple
  }
  if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
     sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1 && hh_check[1,"Owner"] == 1){
    Probs[8] <- Probs[8] + 1 #White Couple, own
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]==
         hh_check[hh_check[,"Relate"]==2,"Race"])==1){
    Probs[9] <- Probs[9] + 1 #Same race couple
  }
  if((sum(hh_check[hh_check[,"Relate"]==1,"Race"]==1)==1 && 
     sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1) |
     sum(hh_check[hh_check[,"Relate"]==2,"Race"]==1)==1 && 
     sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1){
    Probs[10] <- Probs[10] + 1 #White-nonwhite couple
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1 && 
     sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1 && hh_check[1,"Owner"]==1){
    Probs[11] <- Probs[11] + 1 #Non-white couple, own
  }
  if(sum(hh_check[hh_check[,"Relate"]==5,"Gender"]==2)==1 &&
     length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
    Probs[12] <- Probs[12] + 1 #Only mother    
  }
  if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
    Probs[13] <- Probs[13] + 1 #Only one parent   
  }
  if(sum(hh_check[,"Relate"]==3)>=1){
    Probs[14] <- Probs[14] + 1 #Children present  
  }
  if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])>=1){
    Probs[15] <- Probs[15] + 1 #At least one parent present  
  }
  if(sum(hh_check[,"Relate"]==7)>=1){
    Probs[16] <- Probs[16] + 1 #Siblings present  
  }
  if(sum(hh_check[,"Relate"]==9)>=1){
    Probs[17] <- Probs[17] + 1 #Grandchild present  
  }
  if(ifelse(sum(hh_check[,"Relate"]==3)>=1,1,0)+ ifelse(sum(hh_check[,"Relate"]==5)>=1,1,0)+
     ifelse(sum(hh_check[,"Relate"]==9)>=1,1,0)>=2){
    Probs[18] <- Probs[18] + 1 #Three generations present  
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]==
         hh_check[hh_check[,"Relate"]==2,"Age"])==1){
    Probs[19] <- Probs[19] + 1 #Same age couple
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]>hh_check[hh_check[,"Relate"]==2,"Age"])==1 &&
     sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)==1){
    Probs[20] <- Probs[20] + 1 #HH older than spouse, white HH
  }
  if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]==1)==1){
    Probs[21] <- Probs[21] + 1 #Non Hisp HH
  }
  if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]!=1 & hh_check[,"Race"]==1)==1){
    Probs[22] <- Probs[22] + 1 #White HH with hisp origin
  }
}
Probs <- Probs/samp_size
V <- (Probs*(1-Probs))/samp_size
CIntLower <- Probs + (qnorm(0.025)*sqrt(V))
CIntUpper <- Probs - (qnorm(0.025)*sqrt(V))
CInt <- cbind(CIntLower,CIntUpper)


###### 3: Calculate probabilities that depend on relationship variable from complete-case data
Probs_cc <- matrix(0,nrow=22)
n_row_2_cc <- length(which(n_i_cc==1))
n_row_3_cc <- length(which(n_i_cc==2))
n_row_4_cc <- length(which(n_i_cc==3))
samp_size_cc <- matrix(c(n_row_2_cc,n_row_3_cc,n_row_4_cc,rep(n_cc,19)),22,1)
for(kk in 1:n_cc){
  hh_check <- Data_house_cc[kk,(q-p+1):q]
  colnames(hh_check) <- colnames(Data_indiv_cc)
  hh_check <- rbind(hh_check,Data_indiv_cc[which(house_index_cc==kk),])
  hh_check <- data.frame(Owner=Data_house_cc[kk,"Owner"],hh_check)
  if(nrow(hh_check)==2 && hh_check[1,"Race"]==hh_check[2,"Race"]){
    Probs_cc[1] <- Probs_cc[1] + 1 #All same race, n_i = 2
  }
  if(nrow(hh_check)==3 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
     hh_check[2,"Race"]==hh_check[3,"Race"]){
    Probs_cc[2] <- Probs_cc[2] + 1 #All same race, n_i = 3
  }
  if(nrow(hh_check)==4 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
     hh_check[2,"Race"]==hh_check[3,"Race"]&&hh_check[3,"Race"]==hh_check[4,"Race"]){
    Probs_cc[3] <- Probs_cc[3] + 1 #All same race, n_i = 4
  }
  if(sum(hh_check[,"Relate"]==2)>=1){
    Probs_cc[4] <- Probs_cc[4] + 1 #Spouse present
  }
  if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
    Probs_cc[5] <- Probs_cc[5] + 1 #Spouse with white HH
  }
  if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==2)>=1){
    Probs_cc[6] <- Probs_cc[6] + 1 #Spouse with black HH
  }
  if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
     sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
    Probs_cc[7] <- Probs_cc[7] + 1 #White Couple
  }
  if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
     sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1 && hh_check[1,"Owner"] == 1){
    Probs_cc[8] <- Probs_cc[8] + 1 #White Couple, own
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]==
         hh_check[hh_check[,"Relate"]==2,"Race"])==1){
    Probs_cc[9] <- Probs_cc[9] + 1 #Same race couple
  }
  if((sum(hh_check[hh_check[,"Relate"]==1,"Race"]==1)==1 && 
      sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1) |
     sum(hh_check[hh_check[,"Relate"]==2,"Race"]==1)==1 && 
     sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1){
    Probs_cc[10] <- Probs_cc[10] + 1 #White-nonwhite couple
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1 && 
     sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1 && hh_check[1,"Owner"]==1){
    Probs_cc[11] <- Probs_cc[11] + 1 #Non-white couple, own
  }
  if(sum(hh_check[hh_check[,"Relate"]==5,"Gender"]==2)==1 &&
     length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
    Probs_cc[12] <- Probs_cc[12] + 1 #Only mother    
  }
  if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
    Probs_cc[13] <- Probs_cc[13] + 1 #Only one parent   
  }
  if(sum(hh_check[,"Relate"]==3)>=1){
    Probs_cc[14] <- Probs_cc[14] + 1 #Children present  
  }
  if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])>=1){
    Probs_cc[15] <- Probs_cc[15] + 1 #At least one parent present 
  }
  if(sum(hh_check[,"Relate"]==7)>=1){
    Probs_cc[16] <- Probs_cc[16] + 1 #Siblings present  
  }
  if(sum(hh_check[,"Relate"]==9)>=1){
    Probs_cc[17] <- Probs_cc[17] + 1 #Grandchild present  
  }
  if(ifelse(sum(hh_check[,"Relate"]==3)>=1,1,0)+ ifelse(sum(hh_check[,"Relate"]==5)>=1,1,0)+
     ifelse(sum(hh_check[,"Relate"]==9)>=1,1,0)>=2){
    Probs_cc[18] <- Probs_cc[18] + 1 #Three generations present  
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]==
         hh_check[hh_check[,"Relate"]==2,"Age"])==1){
    Probs_cc[19] <- Probs_cc[19] + 1 #Same age couple
  }
  if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]>hh_check[hh_check[,"Relate"]==2,"Age"])==1 &&
     sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)==1){
    Probs_cc[20] <- Probs_cc[20] + 1 #HH older than spouse, white HH
  }
  if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]==1)==1){
    Probs_cc[21] <- Probs_cc[21] + 1 #Non Hisp HH
  }
  if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]!=1 & hh_check[,"Race"]==1)==1){
    Probs_cc[22] <- Probs_cc[22] + 1 #White HH with hisp origin
  }
}
Probs_cc <- Probs_cc/samp_size_cc
V_cc <- (Probs_cc*(1-Probs_cc))/samp_size_cc
CIntLower_cc <- Probs_cc + (qnorm(0.025)*sqrt(V_cc))
CIntUpper_cc <- Probs_cc - (qnorm(0.025)*sqrt(V_cc))
CInt_cc <- cbind(CIntLower_cc,CIntUpper_cc)


###### 4: Calculate probabilities that depend on relationship variable from imputed data
Probs_syn <- matrix(0,nrow=22,ncol=mm)
V_syn <- matrix(0,nrow=22,ncol=mm)
for(k in 1:mm){
  k_imp_house <- dp_imput_house[((n*(k-1))+1):(n*k),]
  k_imp_indiv <- dp_imput_indiv[((N*(k-1))+1):(N*k),]
  new_n_i <- as.numeric(as.character(k_imp_house[,"HHSize"]))
  new_house_index <- rep(c(1:n),new_n_i)
  for(kk in 1:n){
    hh_check <- k_imp_house[kk,(q-p+1):q]
    colnames(hh_check) <- colnames(k_imp_indiv)
    hh_check <- rbind(hh_check,k_imp_indiv[which(new_house_index==kk),])
    hh_check <- data.frame(Owner=k_imp_house[kk,"Owner"],hh_check)
    if(nrow(hh_check)==2 && hh_check[1,"Race"]==hh_check[2,"Race"]){
      Probs_syn[1,k] <- Probs_syn[1,k] + 1 #All same race, n_i = 2
    }
    if(nrow(hh_check)==3 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
       hh_check[2,"Race"]==hh_check[3,"Race"]){
      Probs_syn[2,k] <- Probs_syn[2,k] + 1 #All same race, n_i = 3
    }
    if(nrow(hh_check)==4 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
       hh_check[2,"Race"]==hh_check[3,"Race"]&&hh_check[3,"Race"]==hh_check[4,"Race"]){
      Probs_syn[3,k] <- Probs_syn[3,k] + 1 #All same race, n_i = 4
    }
    if(sum(hh_check[,"Relate"]==2)>=1){
      Probs_syn[4,k] <- Probs_syn[4,k] + 1 #Spouse present
    }
    if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
      Probs_syn[5,k] <- Probs_syn[5,k] + 1 #Spouse with white HH
    }
    if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==2)>=1){
      Probs_syn[6,k] <- Probs_syn[6,k] + 1 #Spouse with black HH
    }
    if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
       sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
      Probs_syn[7,k] <- Probs_syn[7,k] + 1 #White Couple
    }
    if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
       sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1 && hh_check[1,"Owner"] == 1){
      Probs_syn[8,k] <- Probs_syn[8,k] + 1 #White Couple, own
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]==
           hh_check[hh_check[,"Relate"]==2,"Race"])==1){
      Probs_syn[9,k] <- Probs_syn[9,k] + 1 #Same race couple
    }
    if((sum(hh_check[hh_check[,"Relate"]==1,"Race"]==1)==1 && 
        sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1) |
       sum(hh_check[hh_check[,"Relate"]==2,"Race"]==1)==1 && 
       sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1){
      Probs_syn[10,k] <- Probs_syn[10,k] + 1 #White-nonwhite couple
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1 && 
       sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1 && hh_check[1,"Owner"]==1){
      Probs_syn[11,k] <- Probs_syn[11,k] + 1 #Non-white couple, own
    }
    if(sum(hh_check[hh_check[,"Relate"]==5,"Gender"]==2)==1 &&
       length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
      Probs_syn[12,k] <- Probs_syn[12,k] + 1 #Only mother    
    }
    if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
      Probs_syn[13,k] <- Probs_syn[13,k] + 1 #Only one parent   
    }
    if(sum(hh_check[,"Relate"]==3)>=1){
      Probs_syn[14,k] <- Probs_syn[14,k] + 1 #Children present  
    }
    if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])>=1){
      Probs_syn[15,k] <- Probs_syn[15,k] + 1 #At least one parent present 
    }
    if(sum(hh_check[,"Relate"]==7)>=1){
      Probs_syn[16,k] <- Probs_syn[16,k] + 1 #Siblings present  
    }
    if(sum(hh_check[,"Relate"]==9)>=1){
      Probs_syn[17,k] <- Probs_syn[17,k] + 1 #Grandchild present  
    }
    if(ifelse(sum(hh_check[,"Relate"]==3)>=1,1,0)+ ifelse(sum(hh_check[,"Relate"]==5)>=1,1,0)+
       ifelse(sum(hh_check[,"Relate"]==9)>=1,1,0)>=2){
      Probs_syn[18,k] <- Probs_syn[18,k] + 1 #Three generations present  
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]==
           hh_check[hh_check[,"Relate"]==2,"Age"])==1){
      Probs_syn[19,k] <- Probs_syn[19,k] + 1 #Same age couple
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]>hh_check[hh_check[,"Relate"]==2,"Age"])==1 &&
       sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)==1){
      Probs_syn[20,k] <- Probs_syn[20,k] + 1 #HH older than spouse, white HH
    }
    if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]==1)==1){
      Probs_syn[21,k] <- Probs_syn[21,k] + 1 #Non Hisp HH
    }
    if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]!=1 & hh_check[,"Race"]==1)==1){
      Probs_syn[22,k] <- Probs_syn[22,k] + 1 #White HH with hisp origin
    }
  }
  Probs_syn[,k] <- Probs_syn[,k]/samp_size
  V_syn[,k] <- (Probs_syn[,k]*(1-Probs_syn[,k]))/samp_size
}
dp_qbar <- rowMeans(Probs_syn)
dp_b <- apply(Probs_syn,1,var)
dp_ubar <- rowMeans(V_syn)
dp_t <- dp_ubar + (dp_b*(mm+1)/mm) #dp_t <- dp_ubar + (dp_b/mm) for synthetic data
dp_r <- dp_ubar/dp_b
dp_v <- (mm-1)*((1+((mm/(mm+1))*dp_r))^2) #dp_v <- (mm-1)*((1+((mm)*dp_r))^2) for synthetic data
CIntLower_dp <- dp_qbar + (qt(0.025,dp_v)*sqrt(dp_t))
CIntUpper_dp <- dp_qbar - (qt(0.025,dp_v)*sqrt(dp_t))
CInt_syn <- cbind(CIntLower_dp,CIntUpper_dp)


###### 5: Calculate probabilities that depend on relationship variable from imputed data (No strucural zeros model)
Probs_syn_nz <- matrix(0,nrow=22,ncol=mm)
V_syn_nz <- matrix(0,nrow=22,ncol=mm)
for(k in 1:mm){
  k_imp_house <- dp_imput_house_nz[((n*(k-1))+1):(n*k),]
  k_imp_indiv <- dp_imput_indiv_nz[((N*(k-1))+1):(N*k),]
  new_n_i <- as.numeric(as.character(k_imp_house[,"HHSize"]))
  new_house_index <- rep(c(1:n),new_n_i)
  for(kk in 1:n){
    hh_check <- k_imp_house[kk,(q-p+1):q]
    colnames(hh_check) <- colnames(k_imp_indiv)
    hh_check <- rbind(hh_check,k_imp_indiv[which(new_house_index==kk),])
    hh_check <- data.frame(Owner=k_imp_house[kk,"Owner"],hh_check)
    if(nrow(hh_check)==2 && hh_check[1,"Race"]==hh_check[2,"Race"]){
      Probs_syn_nz[1,k] <- Probs_syn_nz[1,k] + 1 #All same race, n_i = 2
    }
    if(nrow(hh_check)==3 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
       hh_check[2,"Race"]==hh_check[3,"Race"]){
      Probs_syn_nz[2,k] <- Probs_syn_nz[2,k] + 1 #All same race, n_i = 3
    }
    if(nrow(hh_check)==4 && hh_check[1,"Race"]==hh_check[2,"Race"]&&
       hh_check[2,"Race"]==hh_check[3,"Race"]&&hh_check[3,"Race"]==hh_check[4,"Race"]){
      Probs_syn_nz[3,k] <- Probs_syn_nz[3,k] + 1 #All same race, n_i = 4
    }
    if(sum(hh_check[,"Relate"]==2)>=1){
      Probs_syn_nz[4,k] <- Probs_syn_nz[4,k] + 1 #Spouse present
    }
    if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
      Probs_syn_nz[5,k] <- Probs_syn_nz[5,k] + 1 #Spouse with white HH
    }
    if(sum(hh_check[,"Relate"]==2)>=1 && sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==2)>=1){
      Probs_syn_nz[6,k] <- Probs_syn_nz[6,k] + 1 #Spouse with black HH
    }
    if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
       sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1){
      Probs_syn_nz[7,k] <- Probs_syn_nz[7,k] + 1 #White Couple
    }
    if(sum(hh_check[,"Relate"]==2 & hh_check[,"Race"]==1)>=1 &&
       sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)>=1 && hh_check[1,"Owner"] == 1){
      Probs_syn_nz[8,k] <- Probs_syn_nz[8,k] + 1 #White Couple, own
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]==
           hh_check[hh_check[,"Relate"]==2,"Race"])==1){
      Probs_syn_nz[9,k] <- Probs_syn_nz[9,k] + 1 #Same race couple
    }
    if((sum(hh_check[hh_check[,"Relate"]==1,"Race"]==1)==1 && 
        sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1) |
       sum(hh_check[hh_check[,"Relate"]==2,"Race"]==1)==1 && 
       sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1){
      Probs_syn_nz[10,k] <- Probs_syn_nz[10,k] + 1 #White-nonwhite couple
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Race"]!=1)==1 && 
       sum(hh_check[hh_check[,"Relate"]==2,"Race"]!=1)==1 && hh_check[1,"Owner"]==1){
      Probs_syn_nz[11,k] <- Probs_syn_nz[11,k] + 1 #Non-white couple, own
    }
    if(sum(hh_check[hh_check[,"Relate"]==5,"Gender"]==2)==1 &&
       length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
      Probs_syn_nz[12,k] <- Probs_syn_nz[12,k] + 1 #Only mother    
    }
    if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])==1){
      Probs_syn_nz[13,k] <- Probs_syn_nz[13,k] + 1 #Only one parent   
    }
    if(sum(hh_check[,"Relate"]==3)>=1){
      Probs_syn_nz[14,k] <- Probs_syn_nz[14,k] + 1 #Children present  
    }
    if(length(hh_check[hh_check[,"Relate"]==5,"Gender"])>=1){
      Probs_syn_nz[15,k] <- Probs_syn_nz[15,k] + 1 #At least one parent present 
    }
    if(sum(hh_check[,"Relate"]==7)>=1){
      Probs_syn_nz[16,k] <- Probs_syn_nz[16,k] + 1 #Siblings present  
    }
    if(sum(hh_check[,"Relate"]==9)>=1){
      Probs_syn_nz[17,k] <- Probs_syn_nz[17,k] + 1 #Grandchild present  
    }
    if(ifelse(sum(hh_check[,"Relate"]==3)>=1,1,0)+ ifelse(sum(hh_check[,"Relate"]==5)>=1,1,0)+
       ifelse(sum(hh_check[,"Relate"]==9)>=1,1,0)>=2){
      Probs_syn_nz[18,k] <- Probs_syn_nz[18,k] + 1 #Three generations present  
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]==
           hh_check[hh_check[,"Relate"]==2,"Age"])==1){
      Probs_syn_nz[19,k] <- Probs_syn_nz[19,k] + 1 #Same age couple
    }
    if(sum(hh_check[hh_check[,"Relate"]==1,"Age"]>hh_check[hh_check[,"Relate"]==2,"Age"])==1 &&
       sum(hh_check[,"Relate"]==1 & hh_check[,"Race"]==1)==1){
      Probs_syn_nz[20,k] <- Probs_syn_nz[20,k] + 1 #HH older than spouse, white HH
    }
    if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]==1)==1){
      Probs_syn_nz[21,k] <- Probs_syn_nz[21,k] + 1 #Non Hisp HH
    }
    if(sum(hh_check[,"Relate"]==1 & hh_check[,"Hisp"]!=1 & hh_check[,"Race"]==1)==1){
      Probs_syn_nz[22,k] <- Probs_syn_nz[22,k] + 1 #White HH with hisp origin
    }
  }
  Probs_syn_nz[,k] <- Probs_syn_nz[,k]/samp_size
  V_syn_nz[,k] <- (Probs_syn_nz[,k]*(1-Probs_syn_nz[,k]))/samp_size
}
dp_qbar_nz <- rowMeans(Probs_syn_nz)
dp_b_nz <- apply(Probs_syn_nz,1,var)
dp_ubar_nz <- rowMeans(V_syn_nz)
dp_t_nz <- dp_ubar_nz + (dp_b_nz*(mm+1)/mm) #dp_t_nz <- dp_ubar_nz + (dp_b_nz/mm) for synthetic data
dp_r_nz <- dp_ubar_nz/dp_b_nz
dp_v_nz <- (mm-1)*((1+((mm/(mm+1))*dp_r_nz))^2) #dp_v_nz <- (mm-1)*((1+((mm)*dp_r_nz))^2) for synthetic data
CIntLower_dp_nz <- dp_qbar_nz + (qt(0.025,dp_v_nz)*sqrt(dp_t_nz))
CIntUpper_dp_nz <- dp_qbar_nz - (qt(0.025,dp_v_nz)*sqrt(dp_t_nz))
CInt_syn_nz <- cbind(CIntLower_dp_nz,CIntUpper_dp_nz)


###### 6: Combine and save!!!
CompareProbs <- cbind(Probs,Probs_cc,dp_qbar,dp_qbar_nz,CInt,CInt_cc,CInt_syn,CInt_syn_nz)
#CompareProbs <- CompareProbs[-3,] #Remove households of size 4 for now
colnames(CompareProbs) = c("Orig-Data Q","CC-Data Q","Model Q","No Struc. Q","Orig-Data L","Orig-Data U",
                           "CC-Data L","CC-Data U","Model L","Model U","No Struc. L","No Struc. U")
write.table(CompareProbs,"Results/CompareProbs.txt",row.names = FALSE)
CompareProbs <- read.table("Results/CompareProbs.txt",header=TRUE)
round(CompareProbs,3)
#round(CompareProbs[,-c(2,6,7)],3)
#library(xtable)
#xtable(round(CompareProbs[,c(3,8,9)],3),digits = 3)
########################## End of Step 3 ########################## 




