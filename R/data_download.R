data_download <-
function(Ticker , start , end = Sys.Date() ){
  N = Ticker
  
  if(length(N) > 1){
  
  for(i in 1:length(N)){
    if(grepl(".", N[i], fixed = TRUE)){
      N2 = gsub("\\.","-",N[i])
      assign(N[i],quantmod::getSymbols(N2, src = "yahoo",from = start-1,to = end, auto.assign = FALSE))
    }else{
      assign(N[i],quantmod::getSymbols(N[i], src = "yahoo", from = start-1,to = end, auto.assign = FALSE))
    }
  }
  
  
  Names = N
  Names2 = c()
  m = 1
  
  for(i in 1:length(Names)){
    if(exists(Names[i])){
      Names2[m] = Names[i]
      m = m+1
    }else{
    }
  }
  
  Names = Names2
  
  for(i in 1:length(Names)){
    Names[i] = paste0(Names[i], "[,6]")
  }
  
  Prices=NA
  for (i in 1:length(Names)){
    Prices = cbind(Prices,eval(parse(text=Names[i])))
  }
  Prices = Prices[,-1]
  colnames(Prices) = Names2
  Prices = as.matrix(Prices)
  
  Prices = Prices[complete.cases(Prices),]
  Names = colnames(Prices)
  
  ret = Prices[-1,]/Prices[-nrow(Prices),] - 1       
  
  if( exists("ret") == FALSE){
    print(paste0("Download more Days!"))
  }
  
  Prices = Prices[-1,]
  Names = Names
  
  if(is.vector(ret) ){
    ret = t(as.matrix(ret))
  }
  
  Returns = ret
  
  Results = list(
    Downloaded_Stock = Names,
    Prices = Prices,
    Returns = Returns
    
  )
  
  }else{
    
    
      if(grepl(".", N, fixed = TRUE)){
        N2 = gsub("\\.","-",N)
        assign(N,quantmod::getSymbols(N2, src = "yahoo",from = start-1,to = end, auto.assign = FALSE))
      }else{
        assign(N,quantmod::getSymbols(N, src = "yahoo", from = start-1,to = end, auto.assign = FALSE))
      }

    

      if(exists(N)){
        Names2 = N
      }else{
      }

    
    Names = Names2
  
    Names = paste0(Names, "[,6]")

    

    Prices = eval(parse(text=Names))
    
    colnames(Prices) = N
    
    Prices = as.matrix(Prices)
    
    Prices = na.omit(Prices)
    
    ret = Prices[-1,]/Prices[-nrow(Prices),] - 1       
    
    if( exists("ret") == FALSE){
      print(paste0("Download more Days!"))
    }
    
    Prices = Prices[-1,]
    
    if(is.vector(ret) ){
      ret = t(as.matrix(ret))
    }
    
    Returns = ret
    
    Results = list(
      Downloaded_Stock = N,
      Prices = Prices,
      Returns = Returns
      
    )
    
    
    
   }
  
  return(Results)
  
}
