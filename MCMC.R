


for(mc in 1:n_iter){
  proc_t <- proc.time()
  #sample structural zeros data
  G_0 <- NULL; M_0 <- NULL
  Data_indiv_struc <- NULL; Data_house_struc <- NULL
  Data_indiv_valid <- NULL; Data_house_valid <- NULL
  for(hh_size in sort(unique(n_i))){
    ss <- which(sort(unique(n_i))==hh_size)
    n_batch <- n_batch_init[ss] + ceiling(n_0[ss]*1.2) #no. of batches of imputations to sample
    n_0[ss] <- 0
    t_0 <- 0; t_1 <- 0
    while(t_1 < length(which(n_i == hh_size))){
      pr_G_t <- lambda[which(level_house[[1]]==hh_size),]*pii #1 is the location of HHsize in level_house
      G_t <- sample(FF,n_batch,prob=pr_G_t,replace=T)
      rep_G_t <- rep(G_t,each=hh_size)
      pr_M_post_t <- omega[rep_G_t,]
      Ran_unif_M_t <- runif(nrow(pr_M_post_t))
      cumul_M_t <- pr_M_post_t%*%upper.tri(diag(ncol(pr_M_post_t)),diag=TRUE)
      M_t <- rowSums(Ran_unif_M_t>cumul_M_t) + 1L
      Data_house_t <- hh_size
      lambda_g_t <- t(lambda[,G_t])
      for(tt in 2:(q-1)){ #q is the HH relate variable, can only take one value
        pr_house_t <- lambda_g_t[,d_k_house_cum[tt]:cumsum(d_k_house)[tt]]
        Ran_unif_t <- runif(nrow(pr_house_t))
        cumul_t <- pr_house_t%*%upper.tri(diag(ncol(pr_house_t)),diag=TRUE)
        level_house_t <- level_house[[tt]]
        Data_house_t <- cbind(Data_house_t,level_house_t[rowSums(Ran_unif_t > cumul_t) + 1L])    
      }
      Data_house_t <- cbind(Data_house_t,1) #relate is always 1 for household head
      Data_indiv_t <- NULL
      phi_m_g_t <- t(phi[,(rep_G_t+((M_t-1)*FF))])
      for(ttt in 1:p){
        pr_indiv_t <- phi_m_g_t[,d_k_indiv_cum[ttt]:cumsum(d_k_indiv)[ttt]]
        Ran_unif_t <- runif(nrow(pr_indiv_t))
        cumul_t <- pr_indiv_t%*%upper.tri(diag(ncol(pr_indiv_t)),diag=TRUE)
        level_indiv_t <- level_indiv[[ttt]]
        Data_indiv_t <- cbind(Data_indiv_t,level_indiv_t[rowSums(Ran_unif_t > cumul_t) + 1L])  
      }
      #comb_to_check <- data.matrix(Data_indiv_t)
      comb_to_check <- Data_indiv_t
      comb_to_check <- matrix(t(comb_to_check),byrow=T,nrow=n_batch)
      comb_to_check <- cbind(Data_house_t[,(q-p+1):q],comb_to_check) #add the household head before check
      check_counter <- checkSZ(comb_to_check,(hh_size+1)) 
      Data_indiv_t <- matrix(t(comb_to_check[,-c(1:p)]),byrow=T,ncol=p)
      
      t_1 <- t_1 + sum(check_counter);
      if(t_1 <= length(which(n_i == hh_size))){
        t_0 <- t_0 + (n_batch - sum(check_counter))
        Data_indiv_struc <- rbind(Data_indiv_struc,Data_indiv_t[which(rep(check_counter,each=hh_size)==0),])
        Data_house_struc <- rbind(Data_house_struc,Data_house_t[which(check_counter==0),])
        Data_indiv_valid <- rbind(Data_indiv_valid,Data_indiv_t[which(rep(check_counter,each=hh_size)==1),])
        Data_house_valid <- rbind(Data_house_valid,Data_house_t[which(check_counter==1),])
        M_0 <- c(M_0,M_t[which(rep(check_counter,each=hh_size)==0)])
        G_0 <- c(G_0,G_t[which(check_counter==0)])
      } else {
        t_needed <- sum(check_counter) - (t_1 - length(which(n_i == hh_size)))
        index_needed <- which(cumsum(check_counter)==t_needed)[1]
        check_counter_needed <- check_counter[1:index_needed]
        t_0 <- t_0 + (length(check_counter_needed) - sum(check_counter_needed))
        Data_indiv_struc <- rbind(Data_indiv_struc,Data_indiv_t[which(rep(check_counter_needed,each=hh_size)==0),])
        Data_house_struc <- rbind(Data_house_struc,Data_house_t[which(check_counter_needed==0),])
        Data_indiv_valid <- rbind(Data_indiv_valid,Data_indiv_t[which(rep(check_counter_needed,each=hh_size)==1),])
        Data_house_valid <- rbind(Data_house_valid,Data_house_t[which(check_counter_needed==1),])
        M_0 <- c(M_0,M_t[which(rep(check_counter_needed,each=hh_size)==0)])
        G_0 <- c(G_0,G_t[which(check_counter_needed==0)])
      }
    }
    n_0[ss] <- n_0[ss] + t_0
  }
  rep_G_0 <- rep(G_0,Data_house_struc[,1])
  row.names(Data_house_struc) <- NULL; row.names(Data_house_valid) <- NULL
  Data_house_struc <- as.data.frame(Data_house_struc)
  Data_house_valid <- as.data.frame(Data_house_valid)
  for(ii in 1:q){
    Data_house_struc[,ii] <- factor(Data_house_struc[,ii],levels=level_house[[ii]])
    Data_house_valid[,ii] <- factor(Data_house_valid[,ii],levels=level_house[[ii]])
  }
  Data_indiv_struc <- as.data.frame(Data_indiv_struc)
  Data_indiv_valid <- as.data.frame(Data_indiv_valid)
  for(iii in 1:p){
    Data_indiv_struc[,iii] <- factor(Data_indiv_struc[,iii],levels=level_indiv[[iii]])
    Data_indiv_valid[,iii] <- factor(Data_indiv_valid[,iii],levels=level_indiv[[iii]])
  }
  colnames(Data_house_struc) <- colnames(Data_house)
  colnames(Data_indiv_struc) <- colnames(Data_indiv)
  colnames(Data_house_valid) <- colnames(Data_house)
  colnames(Data_indiv_valid) <- colnames(Data_indiv)
  
  
  #Free up some memory
  remove(pr_M_post_t); remove(Ran_unif_M_t); remove(cumul_M_t)
  remove(Data_house_t); remove(lambda_g_t); remove(Data_indiv_t); remove(phi_m_g_t)
  remove(pr_house_t); remove(pr_indiv_t); remove(Ran_unif_t); remove(cumul_t)
  remove(comb_to_check); remove(check_counter); remove(check_counter_needed)
  
  
  #Indexes for phi and lambda; data.matrix function won't mess anything up as long as columns are coded as factors
  phi_index <- data.matrix(Data_indiv)+FFF_indiv #has to be within loop for MI
  lambda_index <- data.matrix(Data_house)+FFF_house #has to be within loop  for MI
  
  
  #Change index for missing values
  phi_index[is.na(phi_index)] <- nrow(phi) + 1
  lambda_index[is.na(lambda_index)] <- nrow(lambda) + 1
  
  
  #sample G::: set the probability of NA to be one to make coding easy. Shouldnt affect anything in the multiplications
  pr_G_post <- prGpost(phi_index,lambda_index,rbind(phi,1),rbind(lambda,1),omega,c(pii),FF,SS,n_i)
  Ran_unif_G <- runif(nrow(pr_G_post))
  cumul_G <- pr_G_post%*%upper.tri(diag(ncol(pr_G_post)),diag=TRUE)
  G <- rowSums(Ran_unif_G>cumul_G) + 1L
  remove(pr_G_post); remove(Ran_unif_G); remove(cumul_G); remove(lambda_index)
  
  
  #sample M::: set the probability of NA to be one to make coding easy. Shouldnt affect anything in the multiplications
  rep_G <- rep(G,n_i)
  pr_M_post <- prMpost(phi_index,rbind(phi,1),omega,rep_G,FF,SS)
  Ran_unif_M <- runif(nrow(pr_M_post))
  cumul_M <- pr_M_post%*%upper.tri(diag(ncol(pr_M_post)),diag=TRUE)
  M <- rowSums(Ran_unif_M>cumul_M) + 1L
  remove(pr_M_post); remove(Ran_unif_M); remove(cumul_M); remove(phi_index)
  
  
  #sample phi
  rep_G_all <- c(rep_G,rep_G_0)
  M_all <- c(M,M_0)
  Data_indiv_all <- rbind(Data_indiv,Data_indiv_struc)
  for(gg in 1:SS){
    for(ggg in 1:p){
      phi[d_k_indiv_cum[ggg]:cumsum(d_k_indiv)[ggg],(c(1:FF)+((gg-1)*FF))] <- 
        t(rdirichlet(FF,matrix(a_kdk + table(factor(rep_G_all[which(M_all==gg)],levels=c(1:FF)),
                                             Data_indiv_all[which(M_all==gg),ggg]),nrow=FF)))
    }
  }
  remove(Data_indiv_all)
  
  
  #sample lambda
  G_all <- c(G,G_0)
  Data_house_all <- rbind(Data_house,Data_house_struc)
  for(kk in 1:q){
    lambda[d_k_house_cum[kk]:cumsum(d_k_house)[kk],] <- 
      t(rdirichlet(FF,matrix(a_kdk + table(factor(G_all,levels=c(1:FF)),Data_house_all[,kk]),nrow=FF)))
  }
  remove(Data_house_all)
  
  
  #sample U and pii
  n_f <- matrix(summary(factor(G_all,levels=c(1:FF))),ncol=1)
  U[FF]<-1
  U[1:(FF-1),1] <- rbeta((FF-1),(1L+n_f[1:(FF-1)]),(alpha+(sum(n_f)-cumsum(n_f[-FF]))))
  if(length(which(U[-FF]==1))>0){
    U[which(U[-FF]==1)] <- 0.99999
  }
  one_min_U <- 1L-U
  one_min_U_prod <- c(1,cumprod(one_min_U[1:(FF-1)]))
  pii <- U*one_min_U_prod
  remove(n_f); remove(G_all)
  
  
  #sample V and omega
  M_G <- table(factor(rep_G_all,levels=c(1:FF)),factor(M_all,levels=c(1:SS)))
  n_gm <- as.data.frame(M_G)$Freq
  V[,SS]<-1
  no_V_to_sim <- (FF*(SS-1))
  V[,1:(SS-1)] <- rbeta(no_V_to_sim,(1L+n_gm[1:no_V_to_sim]),
                        (beta + c( matrix(rowSums(M_G),ncol=SS-1,nrow=FF)-
                                     t(apply(M_G,1,cumsum))[,1:SS-1])))
  if(length(which(V[,-SS]==1))>0){
    V[which(V[,-SS]==1)] <- 0.99999
  }
  one_min_V <- 1L-V
  one_min_V_prod <- cbind(1,t(apply(one_min_V[,-SS],1,cumprod)))
  omega <- V*one_min_V_prod
  remove(M_G); remove(rep_G_all); remove(M_all); remove(n_gm)
  
  
  #sample alpha
  alpha <- rgamma(1,shape=(a_alpha+FF-1),rate=(b_alpha-log(pii[FF])))
  
  
  #sample beta
  beta <- rgamma(1,shape=(a_beta+(FF*(SS-1))),rate=(b_beta-sum(log(omega[,SS]))))
  
  
  #check number of occupied clusters
  S.occup <- NULL
  for(occ in sort(unique(G))){
    S.occup <- rbind(S.occup,dim(table(rep_G[which(rep_G==occ)],M[which(rep_G==occ)]))[2])
  }
  
  
  #sample missing X's
  #First household
  if(sum(is.na(NA_house)) > 0){
    lambda_g <- t(lambda[,G])
    for(kkk in 2:q){
      if(length(which(is.na(NA_house[,kkk])==TRUE))>0){
        pr_X_miss_q <- lambda_g[which(is.na(NA_house[,kkk])==TRUE),
                                d_k_house_cum[kkk]:cumsum(d_k_house)[kkk]]
        Ran_unif_miss_q <- runif(nrow(pr_X_miss_q))
        cumul_miss_q <- pr_X_miss_q%*%upper.tri(diag(ncol(pr_X_miss_q)),diag=TRUE)
        level_house_q <- level_house[[kkk]]
        Data_house[which(is.na(NA_house[,kkk])==TRUE),kkk] <-
          level_house_q[rowSums(Ran_unif_miss_q>cumul_miss_q) + 1L]    
      }
    }
  }
  #Now individuals
  if(sum(is.na(NA_indiv)) > 0){
    for(sss in 1:n_miss){
      n_batch_imp <- n_batch_imp_init[sss] + ceiling(n_0_reject[sss]*1.2) #no. of batches of imputations to sample
      n_0_reject[sss] <- 0
      another_index <- which(is.element(house_index,Indiv_miss_index_HH[sss])==TRUE)
      n_another_index <- length(another_index) + 1
      NA_indiv_prop <- NA_indiv[another_index,]
      NA_indiv_prop <- apply(NA_indiv_prop,2,function(x) as.numeric(as.character(x)))
      NA_indiv_prop <- matrix(rep(t(NA_indiv_prop),n_batch_imp),byrow=TRUE,ncol=p)
      rep_G_prop <- rep(rep_G[another_index],n_batch_imp)
      rep_M_prop <- rep(M[another_index],n_batch_imp)
      phi_m_g <- t(phi[,(rep_G_prop+((rep_M_prop-1)*FF))])
      check_counter_sss <- 0;
      while(check_counter_sss < 1){
        Data_indiv_prop <- NA_indiv_prop
        for(kkkk in 1:p){
          if(length(which(is.na(NA_indiv_prop[,kkkk])==TRUE))>0){
            pr_X_miss_p <- matrix(t(phi_m_g[which(is.na(NA_indiv_prop[,kkkk])==TRUE),
                                            d_k_indiv_cum[kkkk]:cumsum(d_k_indiv)[kkkk]]),
                                  nrow=length(which(is.na(NA_indiv_prop[,kkkk])==TRUE)),byrow=T)
            Ran_unif_miss_p <- runif(nrow(pr_X_miss_p))
            cumul_miss_p <- pr_X_miss_p%*%upper.tri(diag(ncol(pr_X_miss_p)),diag=TRUE)
            level_indiv_p <- level_indiv[[kkkk]]
            Data_indiv_prop[is.na(NA_indiv_prop[,kkkk]),kkkk] <- 
              level_indiv_p[rowSums(Ran_unif_miss_p>cumul_miss_p) + 1L]
          }
        }
        #Check edit rules
        comb_to_check <- matrix(t(Data_indiv_prop),nrow=n_batch_imp,byrow=TRUE)
        comb_to_check_HH <-matrix(rep(as.numeric(as.character(Data_house[Indiv_miss_index_HH[sss],(q-p+1):q])),
                                      n_batch_imp),nrow=n_batch_imp,byrow = T)
        comb_to_check <- cbind(comb_to_check_HH,comb_to_check)
        check_counter <- checkSZ(comb_to_check,n_another_index)
        check_counter_sss <- check_counter_sss + sum(check_counter)
        if(length(which(check_counter==1))>0){
          n_0_reject[sss] <- n_0_reject[sss] + length(which(check_counter[1:which(check_counter==1)[1]]==0))
        } else{
          n_0_reject[sss] <- n_0_reject[sss] + n_batch_imp
        }
      }
      Data_indiv[another_index,] <-
        matrix(comb_to_check[which(check_counter==1)[1],-c(1:p)],byrow=TRUE,ncol=p) #remove household head
    }
  }
  
  
  #save and sample missing values
  if(mc > burn_in){
    #PII <- rbind(PII,c(pii))
    ALPHA <- rbind(ALPHA,alpha)
    #G_CLUST <- rbind(G_CLUST,length(unique(G)))
    #M_CLUST <- rbind(M_CLUST,max(S.occup))
    BETA <- rbind(BETA,beta)
    #LAMBDA[(mc-burn_in),] <- c(lambda)
    #OMEGA[(mc-burn_in),] <- c(omega)
    N_ZERO <- rbind(N_ZERO,n_0)
  }
  
  if(sum(mc==M_to_use_mc)==1){
    dp_imput_indiv <- rbind(dp_imput_indiv,Data_indiv)  
    dp_imput_house <- rbind(dp_imput_house,Data_house)
  }
  
  #print
  elapsed_time <- (proc.time() - proc_t)[["elapsed"]]
  cat("Iter=", formatC(mc, width=2, flag=" "),"\t"," ",
      "F=",formatC(length(unique(G)), width=2, flag=" "),"\t",
      "S=",formatC(max(S.occup), width=2, flag=" "),"\t",
      #"alp=",formatC(round(alpha,2), width=1, flag=" "),"\t",
      #"bet=",formatC(round(beta,2), width=1, flag=" "),"\t",
      "n0=",formatC(round(sum(n_0),2),width=1,flag=""),"\t",
      "Eff. n0=",formatC(round((sum(n_0_reject)+sum(n_0)),2),width=1,flag=""),"\t",
      "time= ",formatC(round(elapsed_time,2),width=1,flag=""),"\n", sep = " ")
  
  
}

