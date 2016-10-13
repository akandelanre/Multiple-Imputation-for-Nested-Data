


###########################################################################
###########################################################################
############### MI using NDPMPM: Integrate missing data out ###############
############### Use rejection sampler to sample missing data ##############
############################ Use simulated data ###########################
###########################################################################
###########################################################################

###################################### START ######################################
########################## Step 1: One Time Data Preparation ########################## 
rm(list = ls())
library(DirichletReg)
source("OtherFunctions/OtherFunctions.R")
###### 1: Set global parameters
n <- 20000; F_true <- 5; S_true <- 3; p <- 3; q <- 2;

###### 2a: Set household-level parameters
U <- matrix(rbeta(F_true,1,8),nrow=F_true); U[F_true] <- 1
one_min_U <- 1L-U; one_min_U <- c(1,cumprod(one_min_U[1:(F_true-1)]))
pi_true <- U*one_min_U
G <- sample(c(1:F_true),n,replace=TRUE,prob=pi_true)
d_k_house <- c(2,2) #First one mimics household size
d_k <- 1+cumsum(c(0,d_k_house[-q]))
lambda_true <- matrix(0,nrow=sum(d_k_house),ncol=F_true)
for(g in 1:F_true){
  lambda_true[d_k[1]:cumsum(d_k_house)[1],g] <- t(rdirichlet(1,sample(c(20,100),2,replace=TRUE)))
  lambda_true[d_k[2]:cumsum(d_k_house)[2],g] <- t(rdirichlet(1,sample(c(1,3,10),2,replace=FALSE)))
}

###### 2b: Sample households
X_house <- matrix(0,ncol=q,nrow=n)
for(i in 1:n){
  for(k in 1:q){
    X_house[i,k] <- sample(c(1:d_k_house[k]),1,replace=FALSE,prob=lambda_true[d_k[k]:cumsum(d_k_house)[k],G[i]])
  }
}

###### 3a: Set individual-level parameters
V <- matrix(rbeta((F_true*S_true),1,10),nrow=F_true,ncol=S_true); V[,S_true] <- 1; 
one_min_V <- 1L-V; one_min_V <- cbind(1,as.matrix(one_min_V[,-S_true]))
omega_true <- V*one_min_V
d_k_indiv <- c(2,2,2)
d_k_gm <- 1+cumsum(c(0,d_k_indiv[-p]))
n_i <- X_house[,1]
house_index <- rep(c(1:n),n_i)
n_i_index <- rep(n_i,n_i)
rep_G <- rep(G,n_i)
N <- length(house_index)
M <- matrix(0,nrow=N,ncol=1)
for(j in 1:N){
  M[j] <- sample(c(1:S_true),1,replace=FALSE,prob=omega_true[rep_G[j],])
}
phi_true <- array(0,dim=c(sum(d_k_indiv),S_true,F_true))
for(m in 1:S_true){
  for(g in 1:F_true){
    if(m==1){
      phi_true[d_k_gm[1]:cumsum(d_k_indiv)[1],m,g] <- c(0.2,0.8)
      phi_true[d_k_gm[2]:cumsum(d_k_indiv)[2],m,g] <- c(0.3,0.7)
      phi_true[d_k_gm[3]:cumsum(d_k_indiv)[3],m,g] <- c(0.4,0.6)
    } else if(m==2){
      phi_true[d_k_gm[1]:cumsum(d_k_indiv)[1],m,g] <- c(0.2,0.8)
      phi_true[d_k_gm[2]:cumsum(d_k_indiv)[2],m,g] <- c(0.7,0.3)
      phi_true[d_k_gm[3]:cumsum(d_k_indiv)[3],m,g] <- c(0.4,0.6)
    } else {
      phi_true[d_k_gm[1]:cumsum(d_k_indiv)[1],m,g] <- c(0.4,0.6)
      phi_true[d_k_gm[2]:cumsum(d_k_indiv)[2],m,g] <- c(0.3,0.7)
      phi_true[d_k_gm[3]:cumsum(d_k_indiv)[3],m,g] <- c(0.2,0.8)
    }
  }
}

###### 3b: Sample individuals
X_indiv = matrix(0,ncol=p,nrow=N)
for(j in 1:N){
  for(k in 1:p){
    X_indiv[j,k] <- sample(c(1:d_k_indiv[k]),1,replace=TRUE,prob=phi_true[d_k_gm[k]:cumsum(d_k_indiv)[k],M[j],rep_G[j]])
  }
}

###### 4a: Define Structural Zeros for household 2; Use possible combinations instead of impossibles
X_indiv_check_counter = X_house_check_counter = NULL
for(str_ch in 1:n){
  hh_to_check <- X_indiv[which(house_index==str_ch),]
  hh_to_check <- matrix(t(hh_to_check),nrow=1,byrow=T)
  if(Check_SZ_Other(hh_to_check,h=length(which(house_index==str_ch)))==1){
    X_indiv_check_counter = c(X_indiv_check_counter,which(house_index==str_ch))
    X_house_check_counter = c(X_house_check_counter,str_ch)
  }
}

###### 4b: Select only possible households in final data
X_indiv = X_indiv[X_indiv_check_counter,]
X_house = X_house[X_house_check_counter,]
#G = G[X_house_check_counter]
#M = M[X_indiv_check_counter]

###### 4c: Save!!!
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
source("OtherFunctions/OtherFunctions.R")
X_house <- read.table("Data/X_house.txt",header=TRUE)
X_indiv <- read.table("Data/X_indiv.txt",header=TRUE)
level_house <- list(c(1:2),c(1:2))
level_indiv <- list(c(1:2),c(1:2),c(1:2))
Data_house <- data.frame(X_house)
for(i in 1:ncol(Data_house)){
  Data_house[,i] <- factor(Data_house[,i],levels=level_house[[i]])
}
Data_indiv <- data.frame(X_indiv)
for(i in 1:ncol(Data_indiv)){
  Data_indiv[,i] <- factor(Data_indiv[,i],levels=level_indiv[[i]])
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
set.seed(419)
n_miss <- 0.35*n
House_miss_index <- sample(1:n,n_miss,replace=FALSE)
Indiv_miss_index <- which(is.element(house_index,House_miss_index)==TRUE) #already sorted
for(i in 1:n_miss){
  another_index <- which(is.element(house_index,House_miss_index[i])==TRUE)
  sub_sample <- another_index[sample(length(another_index),sample(length(another_index),1,replace=F),replace=F)]
  if(i <= (0.25*n_miss)){
    Data_indiv[sub_sample,1] <- NA
  }
  if(i > (0.25*n_miss) & i <= (0.5*n_miss)){
    Data_indiv[sub_sample,2] <- NA
  }
  if(i > (0.5*n_miss) & i <= (0.75*n_miss)){
    Data_indiv[sub_sample,3] <- NA
  }
  if(i > (0.75*n_miss) & i <= (0.85*n_miss)){
    Data_indiv[sub_sample,c(1,2)] <- NA
  }
  if(i > (0.85*n_miss) & i <= (0.95*n_miss)){
    Data_indiv[sub_sample,c(2,3)] <- NA
  }
  if(i > (0.95*n_miss)){
    Data_indiv[sub_sample,c(1,3)] <- NA
  }
}
NA_indiv <- Data_indiv; NA_house <- Data_house;
House_miss_index <- sort(House_miss_index)
Data_indiv_cc <- Data_indiv[-Indiv_miss_index,]
Data_house_cc <- Data_house[-House_miss_index,]


###### 4: Calculate observed proportions and number of categories for each variable
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


###### 5: Set parameters for structural zeros
n_batch <- 10000 #sample impossibles in batches before checking constraints


###### 6: Initialize chain
FF <- 15
SS <- 5
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


###### 7: Create empty matrices to save results
dp_imput_house <- dp_imput_indiv <- NULL
ALPHA <- BETA <- PII <- G_CLUST <- M_CLUST <- N_ZERO <- NULL
#LAMBDA <- matrix(0,ncol=(ncol(lambda)*nrow(lambda)),nrow=(n_iter-burn_in))
#OMEGA <- matrix(0,ncol=(ncol(omega)*nrow(omega)),nrow=(n_iter-burn_in))


###### 8: Run MCMC
source("MCMC.R")
MCMC_Results <- list(Data_house_truth=Data_house_truth,Data_indiv_truth=Data_indiv_truth,
                     Data_house_cc=Data_house_cc,Data_indiv_cc=Data_indiv_cc,
                     dp_imput_indiv=dp_imput_indiv,dp_imput_house=dp_imput_house,
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
###### 1: Define parameters and load saved results
mm <- 10;
Data_house_truth <- read.table("Results/Data_house_truth.txt",header=TRUE)
Data_indiv_truth <- read.table("Results/Data_indiv_truth.txt",header=TRUE)
Data_house_cc <- read.table("Results/Data_house_cc.txt",header=TRUE)
Data_indiv_cc <- read.table("Results/Data_indiv_cc.txt",header=TRUE)
dp_imput_house <- read.table("Results/dp_imput_house.txt",header=TRUE)
dp_imput_indiv <- read.table("Results/dp_imput_indiv.txt",header=TRUE)
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
all_biprcomb_indiv_indiv <- combn(p,2)
all_triprcomb_indiv_indiv <- combn(p,3)


###### 2a: Compute marginal probs for individuals
dp_qbarMmarg <- dp_bMmarg <- dp_ubarMmarg <- NULL
margprnomiss <- margvnomiss <- NULL
for(j in 1:p){
  dp_margpr_j <- matrix(0,nrow=mm,ncol=length(unique(Data_indiv_truth[,j])))
  dp_margv_j <- matrix(0,nrow=mm,ncol=length(unique(Data_indiv_truth[,j])))
  for(k in 1:mm){
    k_imp <- dp_imput_indiv[((N*(k-1))+1):(N*k),]
    dp_margpr_j[k,] <- as.data.frame(table(k_imp[,j]))$Freq/dim(k_imp)[1]
    dp_margv_j[k,] <- (dp_margpr_j[k,]*(1-dp_margpr_j[k,]))/dim(k_imp)[1]
  }
  dp_qbarMmarg <- rbind(dp_qbarMmarg,matrix(apply(dp_margpr_j,2,mean),ncol=1))
  dp_bMmarg <- rbind(dp_bMmarg,matrix(apply(dp_margpr_j,2,var),ncol=1))
  dp_ubarMmarg <- rbind(dp_ubarMmarg,matrix(apply(dp_margv_j,2,mean),ncol=1))
  
  margprnomiss_j <- as.data.frame(table(Data_indiv_truth[,j]))$Freq/dim(Data_indiv_truth)[1]
  margprnomiss_j <- matrix(margprnomiss_j,ncol=1)
  margvnomiss_j <- (margprnomiss_j*(1-margprnomiss_j))/dim(Data_indiv_truth)[1]
  margvnomiss_j <- matrix(margvnomiss_j,ncol=1)
  margprnomiss <- rbind(margprnomiss,margprnomiss_j)
  margvnomiss <- rbind(margvnomiss,margvnomiss_j)
}


# Compute Bivariate Probs
dp_qbarMbi <- dp_bMbi <- dp_ubarMbi <- NULL
biprnomiss <- bivnomiss <- NULL
for(j in 1:dim(all_biprcomb_indiv)[2]){
  comb_j <- all_biprcomb_indiv[,j]
  dp_bipr_j <- matrix(0,nrow=mm,ncol=length(table(Data_indiv_truth[,comb_j])))
  dp_biv_j <- matrix(0,nrow=mm,ncol=length(table(Data_indiv_truth[,comb_j])))
  for(k in 1:mm){
    k_imp <- dp_imput_indiv[((N*(k-1))+1):(N*k),]
    dp_bipr_j[k,] <- as.data.frame(table(k_imp[,comb_j]))$Freq/dim(k_imp)[1]
    dp_biv_j[k,] <- (dp_bipr_j[k,]*(1-dp_bipr_j[k,]))/dim(k_imp)[1]
  }
  dp_qbarMbi <- rbind(dp_qbarMbi,matrix(apply(dp_bipr_j,2,mean),ncol=1))
  dp_bMbi <- rbind(dp_bMbi,matrix(apply(dp_bipr_j,2,var),ncol=1))
  dp_ubarMbi <- rbind(dp_ubarMbi,matrix(apply(dp_biv_j,2,mean),ncol=1))
  
  biprnomiss_j <- as.data.frame(table(Data_indiv_truth[,comb_j]))$Freq/dim(Data_indiv_truth)[1]
  biprnomiss_j <- matrix(biprnomiss_j,ncol=1)
  bivnomiss_j <- (biprnomiss_j*(1-biprnomiss_j))/dim(Data_indiv_truth)[1]
  bivnomiss_j <- matrix(bivnomiss_j,ncol=1)
  biprnomiss <- rbind(biprnomiss,biprnomiss_j)
  bivnomiss <- rbind(bivnomiss,bivnomiss_j)
}

# Compute Trivariate Probs
dp_qbarMtri <- dp_bMtri <- dp_ubarMtri <- NULL
triprnomiss <- trivnomiss <- NULL
for(j in 1:dim(all_triprcomb_indiv)[2]){
  combtri_j <- all_triprcomb_indiv[,j]
  dp_tripr_j <- matrix(0,nrow=mm,ncol=length(table(Data_indiv_truth[,combtri_j])))
  dp_triv_j <- matrix(0,nrow=mm,ncol=length(table(Data_indiv_truth[,combtri_j])))
  for(k in 1:mm){
    k_imp <- dp_imput_indiv[((N*(k-1))+1):(N*k),]
    dp_tripr_j[k,] <- as.data.frame(table(k_imp[,combtri_j]))$Freq/dim(k_imp)[1]
    dp_triv_j[k,] <- (dp_tripr_j[k,]*(1-dp_tripr_j[k,]))/dim(k_imp)[1]
  }
  dp_qbarMtri <- rbind(dp_qbarMtri,matrix(apply(dp_tripr_j,2,mean),ncol=1))
  dp_bMtri <- rbind(dp_bMtri,matrix(apply(dp_tripr_j,2,var),ncol=1))
  dp_ubarMtri <- rbind(dp_ubarMtri,matrix(apply(dp_triv_j,2,mean),ncol=1))
  
  triprnomiss_j <- as.data.frame(table(Data_indiv_truth[,combtri_j]))$Freq/dim(Data_indiv_truth)[1]
  triprnomiss_j <- matrix(triprnomiss_j,ncol=1)
  trivnomiss_j <- (triprnomiss_j*(1-triprnomiss_j))/dim(Data_indiv_truth)[1]
  trivnomiss_j <- matrix(trivnomiss_j,ncol=1)
  triprnomiss <- rbind(triprnomiss,triprnomiss_j)
  trivnomiss <- rbind(trivnomiss,trivnomiss_j)
}

margprnomisssim <- margprnomiss
margvnomisssim <- margvnomiss
biprnomisssim <- biprnomiss
bivnomisssim <- bivnomiss
triprnomisssim <- triprnomiss
trivnomisssim <- trivnomiss

dp_qbarmargsim <- dp_qbarMmarg
dp_bmargsim <- dp_bMmarg
dp_ubarmargsim <- dp_ubarMmarg
dp_qbarbisim <- dp_qbarMbi
dp_bbisim <- dp_bMbi
dp_ubarbisim <- dp_ubarMbi
dp_qbartrisim <- dp_qbarMtri
dp_btrisim <- dp_bMtri
dp_ubartrisim <- dp_ubarMtri













###### 3: Combine and save!!!
## Check Convergence to truth
comparemargindiv = round(cbind(dp_qbarmargsim,margprnomisssim),4)
colnames(comparemargindiv) = c("DP","NoMiss")
comparebiindiv = round(cbind(dp_qbarbisim,biprnomisssim),4)
colnames(comparebiindiv) = c("DP","NoMiss")
comparetriindiv = round(cbind(dp_qbartrisim,triprnomisssim),4)
colnames(comparetriindiv) = c("DP","NoMiss")

rbind(comparemargindiv,comparebiindiv,comparetriindiv)


plot(comparemargindiv[,2],comparemargindiv[,1],col="red",lwd=1,
     xlab="Truth",ylab="DP",main="Marginal Probabilities"); grid(nx=20)
abline(0,1)
plot(comparebiindiv[,2],comparebiindiv[,1],col="dark blue",lwd=1,
     xlab="Truth",ylab="DP",main="Bivariate Probabilities"); grid(nx=20)
abline(0,1)
plot(comparetriindiv[,2],comparetriindiv[,1],col="black",lwd=1,
     xlab="Truth",ylab="DP",main="Trivariate Probabilities"); grid(nx=20)
abline(0,1)
title("Individual {Within Household} Probabilities", outer=TRUE)

########################## End of Step 3 ########################## 




