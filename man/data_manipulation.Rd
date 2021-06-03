\name{data_manipulation}
\alias{data_manipulation}
\title{
Data manipulation 
}
\description{
Transformation of the selected stock prices in an expanded form. Simplifies the ggplot visualizations.
}
\usage{
data_manipulation(Data)
}
\arguments{
  \item{Data}{
Time Series the user wants to transform into an extendend veriosion. It simplifies the plotting with the ggplot2 package.
}
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
See also  \code{\link{ggplot2}}
}
\examples{
Tickers = c("AAPL", "AMZN", "FB", "TSLA")
from = as.Date("2020-01-01")

# Get the Data
Data = data_download(Tickers,from)
Prices = Data[["Prices"]]
Returns = Data[["Returns"]]

# Manipulation
Transformed_data = data_manipulation(Prices)
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
