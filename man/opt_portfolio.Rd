\name{opt_portfolio}
\alias{opt_portfolio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal Portfolio
}
\description{
Calculates for a given time series of stocks the optimlal portfolio or the min variance portfolio.
}
\usage{
opt_portfolio(Data, posw = F, minvar = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
 Time Series of Stocks
}
  \item{posw}{
If the user wants positive weights or not
}
  \item{minvar}{
If the user wants the min variance portfolio or the optimal portfolio.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list with weights and standard deviation. In the Optimal Portfolio case it returns also the sharpe ratio and the expected poerfolio return.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bianchi Federico, Corti Matteo, Giay Simone, Passano Andreas
}
\note{
This is not an investment consulting function.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Tickers = c("AAPL", "AMZN", "FB", "TSLA")
from = as.Date("2020-01-01")

# Get the Data
Data = data_download(Tickers,from)
Returns = Data[["Returns"]]

opt_portfolio(Returns)

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
