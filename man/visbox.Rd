\name{visbox}
\alias{visbox}
\title{
Boxplot
}
\description{
visbox() shows the boxplot containing the maximum and minimum values, the value of the 25th and 75th percentile and the median for each column of the dataset, as well as outliers. 
}
\usage{
visbox(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame that the user wants to plot
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
See also \code{\link{visplot}}
See also \code{\link{vizapp}}
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
