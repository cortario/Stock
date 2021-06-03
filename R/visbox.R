visbox <-
function(Data){
  
  Data = as.data.frame(Data)
  
  dates = as.Date( rep(rownames(Data),ncol(Data)))
  Names = c()
  for(i in 1:ncol(Data)){
    Names = c(Names,as.character( rep(colnames(Data)[i],nrow(Data))))
  }
  
  data = data.frame(
    dates = as.Date(dates),
    Ticker = as.character(Names),
    Val = as.numeric(as.vector(as.matrix(Data)))
  )
  
  ggplot(data, aes(x = Ticker, y = Val)) +
    geom_boxplot(size = 1) +
    labs( title = "Boxplots",
          x = "",
          y = "Prices") +
    theme_minimal()
  
}
