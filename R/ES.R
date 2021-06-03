ES <-
function(R, alpha = 0.95, Time = 1,  Ammount = 10000){
  
  R = as.matrix(R)
  R = apply(R,2,sort, decreasing = T)
  
  loss = matrix(NA,nrow = 1,ncol=ncol(R))
  
  losses = c()
  for(i in which(R[,1] == R[alpha*nrow(R),1]):nrow(R)){
    
    losses<- c(losses, abs(Ammount*R[i,]-VaR(R[i,],alpha,Time,Ammount)))
    
  }
  losses = matrix(losses,ncol = ncol(R),byrow = T)
  loss[1,] = apply(losses,2,mean)
  
  
  
  Exp_Short = loss
  colnames(Exp_Short) = colnames(R)
  return(Exp_Short)
}
