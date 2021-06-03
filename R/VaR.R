VaR <-
function(R, alpha = 0.95, Time = 1, Ammount = 10000){
  
  R = as.matrix(R)
  R = apply(R,2,sort, decreasing = T)
  
  quantile = R[alpha*nrow(R),]
  
  Val_at_Risk = abs(Ammount * (apply(R,2,mean) - quantile)*sqrt(Time))
  
  return(Val_at_Risk)
  
  
}
