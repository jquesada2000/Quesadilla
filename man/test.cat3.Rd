% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.cat3.r
\name{test.cat3}
\alias{test.cat3}
\title{Chi-Square test and Fisher test}
\usage{
test.cat3(x, resp)
}
\arguments{
\item{x}{Explanatory qualitative variable wtih k categories}

\item{resp}{Response qualitative variable with 3 categories}
}
\value{
A data.frame with 3xk table with row-percentages, p-value and method (Chi2 or Fisher)
}
\description{
Calculation contingency table and p-value by Chi-Square test and Fisher test
}
\examples{
x <- rbinom(100,2,0.5)
x <- factor(x, levels=c(0,1,2), labels = c('low','Med','High'))
y <- rbinom(100,2,0.5)
y <- factor(y, levels=c(0,1,2), labels = c('low','Med','High'))
test.cat3(x,y)
}
