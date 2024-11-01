

#' @title Chi-Square test and Fisher test
#' @description Calculation contingency table and p-value by Chi-Square test and Fisher test
#' @param x Explanatory qualitative variable wtih k categories
#' @param resp Response qualitative variable with 2 categories
#' @returns A data.frame with 2xk table with row-percentages, p-value and method (Chi2 or Fisher)
#' @examples
#' x <- rbinom(100,2,0.5)
#' x <- factor(x, levels=c(0,1,2), labels = c('low','Med','High'))
#' y <- rbinom(100,1,0.5)
#' y <- factor(x, levels=c(0,1), labels = c('low','High'))
#' test.cat(x,y)
#' @export
test.cat <- function ( x, resp ){

  # calculating the contingency table -----------------
  # Put the variables in order: (explanatory, response)

  t1 <- table ( x, resp)
  p1 <- round ( 100 * prop.table ( t1, 1 ), dig = 1)

  pp <- chisq.test(t1)

  # checking the Chi-square hypothesis ----------------

  esp <- pp$expected
  n_esp5 <- length(esp[esp<5])
  p_esp5 <- n_esp5 / prod(dim(pp$expected))

  if(p_esp5 >= 0.25) {
    pvalor <- round(fisher.test(t1)$p.value,3)
    met <- "Fisher"
  } else {
    pvalor <- round(chisq.test(t1)$p.value,3)
    met <- "Chi2"
  }
  # getting the p value --------------------------------

  pvalor1 <- as.character(pvalor)

  if(is.na(pvalor)){
    pvalor1 <- "-"
  } else {
    if(pvalor < 0.001) pvalor1 <- "<0.001"
  }
  pvalor2 <- matrix("",nrow(t1),2)
  pvalor2[1,1] <- pvalor1
  pvalor2[1,2] <- met

  # saving results in a data frame ---------------------

  An <- paste("n",colnames(t1)[1],sep="")
  Ap <- paste("%",colnames(t1)[1],sep="")

  Bn <- paste("n",colnames(t1)[2],sep="")
  Bp <- paste("%",colnames(t1)[2],sep="")


  aa <- data.frame ( names(t1[,1]),t1[,1],p1[,1],t1[,2],p1[,2],pvalor2[,1],pvalor2[,2] )

  colnames(aa) <- c("cat",An,Ap,Bn,Bp,"p-value","method")

  # removing empty rows -------------
  aa[is.na(aa)] <- ""
  aa
}
