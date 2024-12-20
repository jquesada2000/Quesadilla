% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spearmantest.r
\name{spearmantest}
\alias{spearmantest}
\title{Spearman correlation and 95CI}
\usage{
spearmantest(x, y, conf.level = 0.95)
}
\arguments{
\item{x}{quantitative variable}

\item{y}{quantitative variable}

\item{conf.level}{confidence level for confidence intervals}
}
\value{
A list with AUC and 95CI
}
\description{
Calculates the normalized ranges of two quantitative variables, in order to calculate

the Spearman correlation coefficient and its 95CI, through the cor.test function.
}
\examples{
x <- sample(seq(0,1,by=0.0001), 200 , replace=T)
y <- sample(seq(0,1,by=0.0001), 200 , replace=T)
spearman.test(x,y)
}
