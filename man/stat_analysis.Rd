\name{stat_analysis}
\alias{stat_analysis}
\title{
Statistical analysis
}
\description{
stat_analysis() contains tools for statystical analysis. It returns the mean, tha median and the standard deviation of the given dataset. Moreover, it computes the variance-covariance matrix and the correlation matrix. 
}
\usage{
stat_analysis(Data,weights = NULL)
}
\arguments{
  \item{Data}{
Set of data on which compute the statistical analysis. 
}
  \item{weights}{
Weights for the datasets columns.
}
}
\details{
}
\value{

\item{Mean}{
 average of the different columns of the dataset
}
\item{Standard Deviation}{
standard deviation of the different columns of the dataset
}
\item{Median}{
 value correspondant to the 50th percentile of the different columns of the dataset
}
\item{Variance-Covariance}{
collects the variance of each column of the dataset and the covariance among the different columns
}
\item{Correlation}{
collects the correlation among the different columns of the dataset
}

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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Tickers = c("AAPL", "AMZN", "FB", "TSLA")
from = as.Date("2020-01-01")

# Get the Data
Data = data_download(Tickers,from)
Prices = Data[["Prices"]]
Returns = Data[["Returns"]]

# Plots
visbox(Prices)
visbox(Returns)

# Stat analysis
stat_analysis(Prices)
stat_analysis(Returns)


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
