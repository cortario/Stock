data_manipulation <-
function(Data){
  
  Data = as.data.frame(Data)
  
  dates = as.Date( rep(rownames(Data),ncol(Data)))
  Names = c()
  for(i in 1:ncol(Data)){
    Names = c(Names,as.character( rep(colnames(Data)[i],nrow(Data))))
  }
  
  Du = data.frame(
    dates = as.Date(dates),
    Ticker = as.character(Names),
    Val = as.numeric(as.vector(as.matrix(Data)))
  )
  
  return(Du) 
}
