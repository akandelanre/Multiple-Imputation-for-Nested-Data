


for(mc in 1:n_iter){
  proc_t <- proc.time()
  #sample structural zeros data
  G_0 <- NULL; M_0 <- NULL
  Data_indiv_struc <- NULL; Data_house_struc <- NULL
  Data_indiv_valid <- NULL; Data_house_valid <- NULL
  for(hh_size in sort(unique(n_i))){
    ss <- which(sort(unique(n_i))==hh_size)
    n_batch <- n_batch_init + ceiling(n_0[ss]*prop_batch) #no. of batches of imputations to sample
    n_0[ss] <- 0
    t_0 <- 0; t_1 <- 0
    n_impossibles <- ceiling(length(which(n_i == hh_size))*struc_weight[as.character(hh_size),])
    while(t_1 < n_impossibles){
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
      if(t_1 <= n_impossibles){
        t_0 <- t_0 + (n_batch - sum(check_counter))
        Data_indiv_struc <- rbind(Data_indiv_struc,Data_indiv_t[which(rep(check_counter,each=hh_size)==0),])
        Data_house_struc <- rbind(Data_house_struc,Data_house_t[which(check_counter==0),])
        Data_indiv_valid <- rbind(Data_indiv_valid,Data_indiv_t[which(rep(check_counter,each=hh_size)==1),])
        Data_house_valid <- rbind(Data_house_valid,Data_house_t[which(check_counter==1),])
        M_0 <- c(M_0,M_t[which(rep(check_counter,each=hh_size)==0)])
        G_0 <- c(G_0,G_t[which(check_counter==0)])
      } else {
        t_needed <- sum(check_counter) - (t_1 - n_impossibles)
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
  n_i_0 <- as.numeric(as.character(Data_house_struc[,1]))
  house_index_0 <- rep(c(1:sum(n_0)),n_i_0)
  n_i_index_0 <- rep(n_i_0,n_i_0)
  
  
  #Free up some memory
  remove(pr_M_post_t); remove(Ran_unif_M_t); remove(cumul_M_t)
  remove(Data_house_t); remove(lambda_g_t); remove(Data_indiv_t); remove(phi_m_g_t)
  remove(pr_house_t); remove(pr_indiv_t); remove(Ran_unif_t); remove(cumul_t)
  remove(comb_to_check); remove(check_counter); remove(check_counter_needed)
  
  
  #Indexes for phi and lambda; data.matrix function won't mess anything up as long as columns are coded as factors
  phi_index <- data.matrix(Data_indiv)+FFF_indiv #has to be within loop for MI
  lambda_index <- data.matrix(Data_house)+FFF_house #has to be within loop  for MI
  
  
  #sample G::: set the probability of NA to be one to make coding easy. Shouldnt affect anything in the multiplications
  pr_G_post <- prGpost(phi_index,lambda_index,phi,lambda,omega,c(pii),FF,SS,n_i)
  Ran_unif_G <- runif(nrow(pr_G_post))
  cumul_G <- pr_G_post%*%upper.tri(diag(ncol(pr_G_post)),diag=TRUE)
  G <- rowSums(Ran_unif_G>cumul_G) + 1L
  remove(pr_G_post); remove(Ran_unif_G); remove(cumul_G); remove(lambda_index)
  
  
  #sample M::: set the probability of NA to be one to make coding easy. Shouldnt affect anything in the multiplications
  rep_G <- rep(G,n_i)
  pr_M_post <- prMpost(phi_index,phi,omega,rep_G,FF,SS)
  Ran_unif_M <- runif(nrow(pr_M_post))
  cumul_M <- pr_M_post%*%upper.tri(diag(ncol(pr_M_post)),diag=TRUE)
  M <- rowSums(Ran_unif_M>cumul_M) + 1L
  remove(pr_M_post); remove(Ran_unif_M); remove(cumul_M); remove(phi_index)
  
  
  #sample phi
  rep_G_all <- c(rep_G,rep_G_0)
  M_all <- c(M,M_0)
  #Data_indiv_all <- rbind(Data_indiv,Data_indiv_struc)
  for(gg in 1:SS){
    for(ggg in 1:p){
      phi_count_table <- table(factor(rep_G[which(M==gg)],levels=c(1:FF)),Data_indiv[which(M==gg),ggg])
      for(w_i in 1:length(struc_weight)){
        hh_size <- as.numeric(rownames(struc_weight)[w_i])
        w_i_index <- n_i_index_0==hh_size
        rep_G_0_w_i <- rep_G_0[w_i_index]
        M_0_w_i <- M_0[w_i_index]
        Data_indiv_struc_w_i <- Data_indiv_struc[w_i_index,]
        phi_count_table <- phi_count_table + 
          (table(factor(rep_G_0_w_i[which(M_0_w_i==gg)],levels=c(1:FF)),
                 Data_indiv_struc_w_i[which(M_0_w_i==gg),ggg])/struc_weight[w_i])
      }
      phi[d_k_indiv_cum[ggg]:cumsum(d_k_indiv)[ggg],(c(1:FF)+((gg-1)*FF))] <- 
        t(rdirichlet(FF,matrix(a_kdk + phi_count_table,nrow=FF)))
    }
  }
  #remove(Data_indiv_all)
  
  
  #sample lambda
  G_all <- c(G,G_0)
  #Data_house_all <- rbind(Data_house,Data_house_struc)
  for(kk in 1:q){
    lambda_count_table <- table(factor(G,levels=c(1:FF)),Data_house[,kk])
    for(w_i in 1:length(struc_weight)){
      hh_size <- as.numeric(rownames(struc_weight)[w_i])
      w_i_index <- n_i_0==hh_size
      G_0_w_i <- G_0[w_i_index]
      Data_house_struc_w_i <- Data_house_struc[w_i_index,]
      lambda_count_table <- 
        lambda_count_table + (table(factor(G_0_w_i,levels=c(1:FF)),Data_house_struc_w_i[,kk])/struc_weight[w_i])
    }
    lambda[d_k_house_cum[kk]:cumsum(d_k_house)[kk],] <- t(rdirichlet(FF,matrix(a_kdk + lambda_count_table,nrow=FF)))
  }
  #remove(Data_house_all)
  
  
  #sample U and pii
  n_f <- matrix(summary(factor(G,levels=c(1:FF))),ncol=1)
  for(w_i in 1:length(struc_weight)){
    hh_size <- as.numeric(rownames(struc_weight)[w_i])
    w_i_index <- n_i_0==hh_size
    G_0_w_i <- G_0[w_i_index]
    n_f <- n_f + (matrix(summary(factor(G_0_w_i,levels=c(1:FF))),ncol=1)/struc_weight[w_i])
  }
  U[FF]<-1
  U[1:(FF-1),1] <- rbeta((FF-1),(1L+n_f[1:(FF-1)]),(alpha+(sum(n_f)-cumsum(n_f[-FF]))))
  if(length(which(U[-FF]==1))>0){
    U[which(U[-FF]==1)] <- 0.99999
  }
  one_min_U <- 1L-U
  one_min_U_prod <- c(1,cumprod(one_min_U[1:(FF-1)]))
  pii <- U*one_min_U_prod
  remove(n_f);
  
  
  #sample V and omega
  M_G <- table(factor(rep_G,levels=c(1:FF)),factor(M,levels=c(1:SS)))
  for(w_i in 1:length(struc_weight)){
    hh_size <- as.numeric(rownames(struc_weight)[w_i])
    w_i_index <- n_i_index_0==hh_size
    rep_G_0_w_i <- rep_G_0[w_i_index]
    M_0_w_i <- M_0[w_i_index]
    M_G <- M_G + (table(factor(rep_G_0_w_i,levels=c(1:FF)),factor(M_0_w_i,levels=c(1:SS)))/struc_weight[w_i])
  }
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
  remove(M_G); remove(n_gm)
  
  #sample alpha
  alpha <- rgamma(1,shape=(a_alpha+FF-1),rate=(b_alpha-log(pii[FF])))
  
  
  #sample beta
  beta <- rgamma(1,shape=(a_beta+(FF*(SS-1))),rate=(b_beta-sum(log(omega[,SS]))))
  
  
  #check number of occupied clusters
  S_occup <- NULL
  for(occ in sort(unique(G_all))){
    S_occup <- rbind(S_occup,dim(table(rep_G_all[which(rep_G_all==occ)],M_all[which(rep_G_all==occ)]))[2])
  }
  remove(rep_G_all); remove(M_all); 
  
  
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
    phi_m_g <- matrix(0,nrow=N,ncol=dim(phi)[1])
    for(jjj in 1:N){
      phi_m_g[jjj,] <- phi[,(rep_G[jjj]+((M[jjj]-1)*FF))]
    }
    for(kkkk in nonstruc_zero_variables){
      if(length(which(is.na(NA_indiv[,kkkk])==TRUE))>0){
        pr_X_miss_p <- phi_m_g[which(is.na(NA_indiv[,kkkk])==TRUE),d_k_indiv_cum[kkkk]:cumsum(d_k_indiv)[kkkk]]
        Ran_unif_miss_p <- runif(nrow(pr_X_miss_p))
        cumul_miss_p <- pr_X_miss_p%*%upper.tri(diag(ncol(pr_X_miss_p)),diag=TRUE)
        level_indiv_p <- level_indiv[[kkkk]]
        Data_indiv[which(is.na(NA_indiv[,kkkk])==TRUE),kkkk] <- level_indiv_p[rowSums(Ran_unif_miss_p>cumul_miss_p) + 1L]
      }
    }
    
    SampleNew <- sample(c("TRUE","FALSE"),1,prob=hybrid_prob,replace=FALSE)
    if(SampleNew){
      n_batch_imp <- n_batch_imp_init + ceiling(n_0_reject*prop_batch) #no. of batches of imputations to sample
      n_0_reject[] <- 0
      for(hh_size in sort(unique(n_i))){
        n_0_reject_hh <- n_0_reject[n_i_miss==hh_size]
        Indiv_miss_index_HH_hh <- Indiv_miss_index_HH[n_i_miss==hh_size]
        Indiv_miss_index_hh <- which(is.element(house_index,Indiv_miss_index_HH_hh)==TRUE)
        n_batch_imp_hh <- n_batch_imp[n_i_miss==hh_size]
        n_miss_hh <- length(Indiv_miss_index_HH_hh)
        n_batch_imp_hh_cum <- c(0,cumsum(n_batch_imp_hh)[-n_miss_hh])
        n_i_miss_index_hh <- rep(1:n_miss_hh,n_batch_imp_hh)
        NA_indiv_prop <- Data_indiv[Indiv_miss_index_hh,]
        NA_indiv_prop[,struc_zero_variables] <- NA_indiv[Indiv_miss_index_hh,struc_zero_variables]
        NA_indiv_prop <- apply(NA_indiv_prop,2,function(x) as.numeric(as.character(x)))
        NA_indiv_prop <- matrix(t(NA_indiv_prop),byrow=T,ncol=(p*hh_size))
        NA_indiv_prop <- apply(NA_indiv_prop,2,function(x) rep(x,n_batch_imp_hh))
        NA_indiv_prop <- matrix(t(NA_indiv_prop),byrow=T,ncol=p)
        rep_G_prop <- apply(matrix(rep_G[Indiv_miss_index_hh],byrow=T,ncol=hh_size),2,function(x) rep(x,n_batch_imp_hh))
        rep_G_prop <- c(t(rep_G_prop))
        M_prop <- apply(matrix(M[Indiv_miss_index_hh],byrow=T,ncol=hh_size),2,function(x) rep(x,n_batch_imp_hh))
        M_prop <- c(t(M_prop))
        phi_m_g <- t(phi[,(rep_G_prop+((M_prop-1)*FF))])
        Data_house_prop <- Data_house[Indiv_miss_index_HH_hh,(q-p+1):q]
        Data_house_prop <- apply(Data_house_prop,2,function(x) rep(as.numeric(as.character(x)),n_batch_imp_hh))
        check_counter <- rep(0,n_miss_hh)
        check_counter_index <- rep(0,n_miss_hh)
        while(sum(check_counter) < n_miss_hh){
          Data_indiv_prop <- NA_indiv_prop
          for(kkkk in struc_zero_variables){
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
          comb_to_check <- matrix(t(Data_indiv_prop),ncol=(p*hh_size),byrow=TRUE)
          comb_to_check <- cbind(Data_house_prop,comb_to_check)
          check_counter_batch <- checkSZ(comb_to_check,(hh_size+1))
          for(sss in 1:n_miss_hh){
            check_counter[sss] <- ifelse(sum(check_counter_batch[which(n_i_miss_index_hh==sss)])>=1,1,0)
            check_counter_index[sss] <- n_batch_imp_hh_cum[sss] + 
              which(check_counter_batch[which(n_i_miss_index_hh==sss)]==1)[1]
            n_0_reject_hh[sss] <- 
              n_0_reject_hh[sss] + ifelse(check_counter[sss]==0,n_batch_imp_hh[sss],
                                          (which(cumsum(check_counter_batch[which(n_i_miss_index_hh==sss)])==1)[1]-1))
          }
          check_counter_index[is.na(check_counter_index)] <- 0
        }
        Data_indiv[Indiv_miss_index_hh,] <- matrix(t(comb_to_check[check_counter_index,-c(1:p)]),byrow=T,ncol=p)
        n_0_reject[n_i_miss==hh_size] <- n_0_reject_hh
      }
    } else {
      for(sss in 1:n_miss){
        another_index <- which(is.element(house_index,Indiv_miss_index_HH[sss])==TRUE)
        n_another_index <- length(another_index)
        post_prop_indiv_sss <- Post_prop_indiv[[sss]]
        FFF_indiv_prop <- matrix(cumsum(c(0,d_k_indiv[,-p])),ncol=p,nrow=nrow(post_prop_indiv_sss),byrow=T)
        phi_index_prop <- data.matrix(post_prop_indiv_sss) + FFF_indiv_prop
        G_prop <- rep_G[another_index]; M_prop <- M[another_index];
        G_prop <- rep(G_prop,n_prop); M_prop <- rep(M_prop,n_prop)
        pi_prop <- t(as.matrix(prHH(phi_index_prop,phi,c(G_prop),c(M_prop),FF,h=(n_another_index-1))))
        pi_prop <- pi_prop/sum(pi_prop) #renormalize
        Ran_unif_miss_prop <- runif(nrow(pi_prop))
        cumul_miss_prop <- pi_prop%*%upper.tri(diag(ncol(pi_prop)),diag=TRUE)
        index_prop <- rowSums(Ran_unif_miss_prop>cumul_miss_prop) + 1L
        Data_indiv[another_index,] <- post_prop_indiv_sss[(1:n_another_index+(n_another_index*(index_prop-1))),]
      }
    }
    
    #free up more memory
    remove(comb_to_check); remove(cumul_miss_p); remove(Data_house_prop); remove(Data_indiv_prop);
    remove(NA_indiv_prop); remove(phi_m_g); remove(pr_X_miss_p); remove(check_counter_batch);
    remove(M_prop); remove(n_i_miss_index_hh); remove(Ran_unif_miss_p); remove(rep_G_prop)
  }
  
  
  #save and sample missing values
  if(mc > burn_in){
    #PII <- rbind(PII,c(pii))
    ALPHA <- rbind(ALPHA,alpha)
    #G_CLUST <- rbind(G_CLUST,length(unique(G)))
    #M_CLUST <- rbind(M_CLUST,max(S_occup))
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
  cat("Iter:", formatC(mc, width=2, flag=" "),"\t"," ",
      "F:",formatC(length(unique(G_all)), width=2, flag=" "),"\t",
      "S:",formatC(max(S_occup), width=2, flag=" "),"\n",
      #"alpha:",formatC(round(alpha,2), width=2, flag=" "),"\t",
      #"beta:",formatC(round(beta,2), width=2, flag=" "),"\t",
      "n0:",sum(n_0),"\t",
      "Eff n0:",(sum(n_0_reject)+sum(n_0)),"\t",
      "True n0:",(sum(n_0_reject)+sum(n_0/struc_weight)),"\n",
      "Time= ",elapsed_time,"\n","\n",sep = " ")
  remove(G_all)
}

