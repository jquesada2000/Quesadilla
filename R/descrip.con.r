

  #' Quesadilla package
  #'
  #' This package contains some functions applied in Biostatistics
  #'
  "_PACKAGE"
  #' @title Descriptive quantitative explanatory variables
  #' @description Calculation of descriptive statistics for quantitative variable.
  #' @param var Data vector of quantitative variable
  #' @returns A data.frame with n valid, min, max, mean. median, sd and iqr of each variable.
  #' @examples
  #' x <- rnorm(100, mean = 10, sd = 15)
  #' descrip.con(x)
  #' @export
  descrip.con <- function ( var ) {
  
  if ( is.factor(var) == TRUE )
  { stop ( "Error: The variable is a factor" ) }
  
  nvalido = sum(!is.na(var))
  minimo = round(min(var, na.rm=TRUE),1)
  maximo = round(max(var, na.rm=TRUE),1)
  mean = round(mean (var, na.rm=TRUE),2)
  median = round(median(var, na.rm=TRUE),2)
  
  sd = round(sd (var, na.rm=TRUE),2)
  iqr = round(IQR (var, na.rm=TRUE),2)
  
  data.frame ( nvalid=nvalido, Min=minimo, Max=maximo,Mean=mean,SD=sd, Median=median,IQR=iqr)
  }