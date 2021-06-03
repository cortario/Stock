\name{ES}
\alias{ES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expected Shortfall
}
\description{
Returns the Expected Shortfall for each stock or portfolio
}
\usage{
ES(R, alpha = 0.95, Time = 1, Ammount = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
Returns Matrix or Dataframe
}
  \item{alpha}{
Confindence Interval
}
  \item{Time}{
Time Horizon
}
  \item{Ammount}{
Monetary Ammount
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a Matrix containing the Expected Shortfall.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bianchi Federico, Corti Matteo, Giay Simone, Passano Andreas
}
\note{
%%  ~~further notes~~
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

Returns = Data[["Returns"]]

ES(Returns)
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
