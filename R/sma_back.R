sma_back <-
function(P,sma_s= 5,sma_l = 10){

  Names = colnames(P)
  
  N = ncol(P)
  sma1 = apply(P,2,TTR::SMA,sma_s)
  sma2 = apply(P,2,TTR::SMA,sma_l)
  
  Positions = ifelse(sma1 >= sma2, 1, -1)
  Trades = ifelse(Positions ==1 ,"Buy","Sell")
  
  idx = list()
  for(i in 1:N){
    idx[[paste0(Names[i])]] = which( ifelse(diff(Positions[,i]) == 0,0,1)==1)+1
  }
  
  for(i in 1:N){
    idx[[paste0(Names[i])]] = c(sma_l,idx[[paste0(Names[i])]],nrow(P))
  }
  
  Ret = list()
  for(i in 1:N){
    for(k in 2:length(idx[[paste0(Names[i])]])){
      Ret[[paste0(Names[i])]][k-1] = P[idx[[paste0(Names[i])]][k],i] / P[idx[[paste0(Names[i])]][k-1],i] -1
    }
  }
  
  Algoret = list()
  for(i in 1:N){
    Algoret[[paste0(Names[i])]] = Ret[[paste0(Names[i])]] * Positions[idx[[paste0(Names[i])]][-length(idx[[paste0(Names[i])]])],i]
  }
  
  Trades = list()
  for(i in 1:N){
    Trades[[paste0(Names[i])]] = ifelse(Positions[idx[[paste0(Names[i])]][-length(idx[[paste0(Names[i])]])],i] == 1,"Buy","Sell") 
  }
  
  Annualized_Returns = c()
  sd = c()
  Sharpe = c()
  for(i in 1:N){
    Annualized_Returns[i] = ((mean(Algoret[[paste0(Names[i])]])+1)^252-1)
    sd[i] = sd(Algoret[[paste0(Names[i])]])
    Sharpe[i] = (Annualized_Returns[i])/sd[i]
  }
  
  guessed = c()
  for(i in 1:N){
    guessed[i] = sum(Algoret[[paste0(Names[i])]] >= 0)/ length(Algoret[[paste0(Names[i])]])
  }
  
  Pr = P
  AR = Algoret
  
  
  
  Today = c()
  for(i in 1:N){
    Today[i] = tail(Trades[[paste0(Names[i])]],1)
  }
  
  return( rbind(Sharpe = Sharpe,
                sd = sd, 
                Annualized_Returns = Annualized_Returns,
                guessed = guessed,
                Today=Today )
  )
  
}
