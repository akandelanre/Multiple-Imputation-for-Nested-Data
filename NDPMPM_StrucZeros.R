


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
n <- 20000; F_true <- 5; S_true <- 2; p <- 3; q <- 2;

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
    phi_true[d_k_gm[1]:cumsum(d_k_indiv)[1],m,g] <- c(0.2,0.8)
    phi_true[d_k_gm[2]:cumsum(d_k_indiv)[2],m,g] <- c(0.3,0.7)
    if(m==1){
      phi_true[d_k_gm[3]:cumsum(d_k_indiv)[3],m,g] <- c(0.4,0.6)
    } else{
      phi_true[d_k_gm[3]:cumsum(d_k_indiv)[3],m,g] <- c(0.6,0.4)
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
n_miss <- 0.30*n
House_miss_index <- NULL
Indiv_miss_index_HH <- sample(1:n,n_miss,replace=FALSE)
Indiv_miss_index <- which(is.element(house_index,Indiv_miss_index_HH)==TRUE) #already sorted
O_indiv <- matrix(1,ncol=p,nrow=N)
colnames(O_indiv) <- colnames(Data_indiv)
O_indiv[Indiv_miss_index,] <- 1
Data_indiv[O_indiv==0] <- NA
NA_indiv <- Data_indiv; NA_house <- Data_house;
Indiv_miss_index_HH <- sort(Indiv_miss_index_HH)
Data_indiv_cc <- Data_indiv[-Indiv_miss_index,]
Data_house_cc <- Data_house[-Indiv_miss_index_HH,]


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




