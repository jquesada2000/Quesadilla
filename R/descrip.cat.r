
  #' Quesadilla package
  #'
  #' This package contains some functions applied in Biostatistics
  #'
  "_PACKAGE"
  #' @title Descriptive qualitative explanatory variables
  #' @description Calculation of descriptive statistics for qualitative variable.
  #' @param var Data vector of qualitative variable
  #' @returns A data.frame with number(n) and proportion(p) of each category
  #' @examples
  #' x <- rbinom(100,2,0.5)
  #' x <- factor(x, levels=c(0,1,2), labels = c('low','Med','High'))
  #' descrip.cat(x)
  #' @export
 descrip.cat <- function ( var ){
  t1 = table ( var )
  categ <- names(t1)
  p1 = prop.table( t1 )
  porc = round ( 100 * p1, dig = 1 )
  data.frame (cat = categ, n = as.vector(t1),p = as.vector(porc) )
}



