
#' @title Spearman correlation and 95CI
#' @description Calculates the normalized ranges of two quantitative variables, in order to calculate
#' @description the Spearman correlation coefficient and its 95CI, through the cor.test function.
#' @param x quantitative variable
#' @param y quantitative variable
#' @param conf.level confidence level for confidence intervals
#' @returns A list with AUC and 95CI
#' @examples
#' x <- sample(seq(0,1,by=0.0001), 200 , replace=T)
#' y <- sample(seq(0,1,by=0.0001), 200 , replace=T)
#' spearman.test(x,y)
#' @export
spearmantest <- function(x, y, conf.level = 0.95) {

  RIN <- function(x){qnorm((rank(x) - 0.5)/(length(rank(x))))}
  x_rin <- RIN(x)
  y_rin <- RIN(y)
  c(Rho= cor.test(x,y, method = "spearman")$estimate, 'RIN corrected CI'= cor.test(x_rin,y_rin)$conf.int[1:2])

  }




