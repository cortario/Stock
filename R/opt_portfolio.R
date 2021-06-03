opt_portfolio <-
function(Data,posw = F,minvar = T){
  
  if(minvar){
    N = ncol(Data)
    
    # Min - Var,Covar
    dvec = rep(0,N)
    Dmat = 2 * cov(Data) 
    
    # Vincoli
    if(posw){
      Amat =  cbind( matrix(rep(1,N), ncol = 1) , diag(1,N))
      bvec = c(1,rep(0,N))
    }else{
      Amat <- matrix(rep(1,N), ncol = 1)
      bvec = c(1)
    }
    
    # Solution
    result <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 
    w = result$solution
    sd = sqrt(result$value)
    
    return(list(weights = w, s.deviation = sd))
    
  }else{
    
    N = ncol(Data)
    
    Val = 10000
    
    mu_options = apply(Data,2,mean)
    
    mu_portfolio <- seq(min(mu_options)+0.00001,max(mu_options)-0.00001,length = Val)
    
    if(posw){
      A_equality =  cbind( matrix(rep(1,N), ncol = 1) , diag(1,N))
      bvec = c(1,rep(0,N))
    }else{
      A_equality <- matrix(rep(1,N), ncol = 1)
      bvec = c(0)
    }
    
    deviazioni_standard = NULL
    ritorno_medio_p= NULL
    weights_matrix =  matrix(0, nrow=Val, ncol = N )
    
    dvec = matrix(0,nrow=N)
    Dmat = 2*cov(Data)
    
    for( i in 1:length(mu_portfolio)){
      Amat_p = cbind(A_equality, mu_options)
      bvec_p = c(bvec,mu_portfolio[i])
      result_p <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat_p, bvec = bvec_p, meq = 1)
      weights_p <- round(result_p$solution, digits = 4)
      ritorno_medio_p[i] = t(as.matrix(weights_p)) %*% as.matrix(mu_options)
      deviazioni_standard[i]= sqrt(result_p$value)
      weights_matrix[i,] <- round(result_p$solution, digits = 4)
    }
    
    Sharpe_ratio <- ( mu_portfolio) / deviazioni_standard 
    sharpe_location <- which.max(Sharpe_ratio)
    
    tangency_weights <- weights_matrix[sharpe_location,]
    tangency_sharpe <- max(Sharpe_ratio)
    tangency_ret <- ritorno_medio_p[sharpe_location]
    tangency_sd = deviazioni_standard[sharpe_location]
    
    
    return(list(
      weights = tangency_weights,
      sharpe = tangency_sharpe,
      expected_return = tangency_ret,
      s_deviation = tangency_sd
    ))
    
  }
  
}
