

## Quick function to sort a HH vector of within household data by relate variable
quick_sort_HH <- function(X=x,P=p,R=r){# X =vector, p =no of variables, r =col index of relate var 
  XX = matrix(X,ncol=P,byrow = TRUE)
  XX = XX[order(XX[,R]),]
  return(matrix(t(XX),nrow=1))
}

Check_SZ <- function(Data,h){
  if(h==1){
    if(Data[1,1] == 2 & Data[1,2] == 2 & Data[1,3] == 1 |
       Data[1,1] == 2 & Data[1,2] == 2 & Data[1,3] == 2){
      return(0)
    } else {return(1)} 
  }
  
  if(h==2){
    count <- 0
    for(i in 1:h){
      if(Data[i,1] == 2 & Data[i,2] == 1 & Data[i,3] == 2 |
         Data[i,1] == 2 & Data[i,2] == 2 & Data[i,3] == 2){
        count <- count + 1
      }
    }
    if(count > 0){
      return(0) 
    } else {return(1)} 
  }
}


Check_SZ_Other <- function(Data,h){
  check_result <- matrix(0,nrow=nrow(Data))
  for(i in 1:nrow(Data)){
    Data_i <- matrix(Data[i,],nrow=h,byrow=T)
    check_result[i] <- Check_SZ(Data_i,h)
  }
  return(check_result)
}


