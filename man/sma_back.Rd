\name{sma_back}
\alias{sma_back}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SMA Backtest
}
\description{
Test for given Stocks and for given periods the SMA strategy
}
\usage{
sma_back(P, sma_s = 5, sma_l = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
Time Series of Prices
}
  \item{sma_s}{
Short SMA period
}
  \item{sma_l}{
Long SMA period
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a Matrix with the stocks Sharpe Ratio, Standard deviation, Annualized returns, the guessed positions and the buy/sell position the user should take.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bianchi Federico, Corti Matteo, Giay Simone, Passano Andreas
}
\note{
This is not an investment consulting function. No guarantees are taken from the authors.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

Tickers = c("AAPL", "AMZN", "FB", "TSLA")
from = as.Date("2020-01-01")

# Get the Data
Data = data_download(Tickers,from)

Prices = Data[["Prices"]]

sma_back(Prices)



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
