visplot <-
function(Stock,title = "Daily Stock Prices"){
  
  Data = as.data.frame(Stock)
  
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
  
  ggplot(data, aes(x = dates, y = Val, colour = Ticker)) +
    geom_line(size = 1) +
    labs( title = title,
          x = "Dates",
          y = "Prices") +
    theme_minimal()
  
}
