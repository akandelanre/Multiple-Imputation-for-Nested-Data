

## Quick function to sort a HH vector of within household data by relate variable
quick_sort_HH <- function(X=x,P=p,R=r){# X =vector, p =no of variables, r =col index of relate var 
  XX = matrix(X,ncol=P,byrow = TRUE)
  XX = XX[order(XX[,R]),]
  return(matrix(t(XX),nrow=1))
}

Check_SZ_Other <- function(Data){
  if(length(Data)==3){
    Data <- t(as.matrix(Data))
  }
  h <- nrow(Data)
  count <- 0
  for(i in 1:h){
    if(Data[i,1] == 2 & Data[i,2] == 2 & Data[i,3] == 1 |
       Data[i,1] == 2 & Data[i,2] == 2 & Data[i,3] == 2){
      count <- count + 1
    }
  }
  if(count < h){
    return(1) #households that pass
  } else {return(0)} 
}
