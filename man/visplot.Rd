\name{visplot}
\alias{visplot}
\title{
Plot historycal prices
}
\description{
It plots the price of the chosen stocks in the selected window of time.
}
\usage{
visplot(Stock,title = "Daily Stock Prices")
}
\arguments{
  \item{Stock}{
Stock prices (time-series) that the user wants to plot
}
  \item{title}{
A text that becaomes the title of the plot.
}
}
\details{
}
\value{
%% ...
}
\references{

}
\author{
Bianchi Federico, Corti Matteo, Giay Simone, Passano Andreas
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{visbox}}
See also \code{\link{vizapp}}
}
\examples{
Tickers = c("AAPL", "AMZN", "FB", "TSLA")
from = as.Date("2020-01-01")

# Get the Data
Data = data_download(Tickers,from)
Prices = Data[["Prices"]]

# Plots
visplot(Prices)
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
