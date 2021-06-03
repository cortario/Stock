\name{data_download}
\alias{data_download}
\title{
Time series of stock Prices and Returns 
}
\description{
data_download() downloads from Yahoo Finance the desired stocks prices and it computes the daily returns. These data are then stored in two time series (Prices and Returns). It is also possible to see which Tickers have actually been downloaded.
}
\usage{
data_download(Ticker, start, end = Sys.Date())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ticker}{
Vector of stock tickers the user wish to downaload
}
  \item{start}{
Starting date
}
  \item{end}{
End date. If not specified it's used the system date. 
}
}
\value{
\item{Names }{Downloaded tickers}
\item{Prices }{Time series of daily prices}
\item{Returns }{Time series of daily adjusted returns}
}

\author{
Bianchi Federico, Corti Matteo, Giay Simone, Passano Andreas
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{quantmod}}
See also \code{\link{vizapp}}
}
\examples{
Tickers = c("AAPL", "AMZN", "FB", "TSLA")
from = as.Date("2020-01-01")

# Get the Data
Data = data_download(Tickers,from)

Names = Data[["Downloaded_Stock"]]
Prices = Data[["Prices"]]
Returns = Data[["Returns"]]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
