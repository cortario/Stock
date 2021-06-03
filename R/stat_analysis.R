stat_analysis <-
function(Data, weights = NULL){
  if(!is.null(weights)){
    Data = Data %*% as.matrix(weights)
  }
  
  Data = as.data.frame(Data)
  
  d_mean = apply(Data,2,mean)
  d_sd = apply(Data,2,sd)
  
  d_quantile = apply(Data,2,quantile)
  
  
  Stat = rbind( d_sd,d_mean, d_quantile)
  rownames(Stat) = c("S.d.","Mean",  "0%","25%","50%","75%","100%")
  
  Varcovar = var(Data)
  Corr = cor(Data)
  
  Results = list(Statistical_Data = Stat, 
                 Variance_Covariance = Varcovar,
                 Correlation = Corr)
  
  return(Results)
  
  
}
