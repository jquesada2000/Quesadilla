





#' @title Error Rate in tables 2x2
#' @description Error Rate in tables 2x2 Observed and Predicted values
#' @param obs Data vector of observed values
#' @param pred Data vector of predicted values
#' @returns A list with accuracy and error rates
#' @examples
#' x <- rbinom(100,2,0.5)
#' x <- factor(x, levels=c(0,1,2), labels = c('low','Med','High'))
#' y <- rbinom(100,2,0.5)
#' y <- factor(y, levels=c(0,1,2), labels = c('low','Med','High'))
#' ErrorRate(x,y)
#' @export
ErrorRate = function ( obs, pred ){

  if ( length(obs) != length(pred) )
  { stop ( "The variables must have same length" ) }

  ## Table
  tt = table( obs, pred )

  ## accuracy and error rate
  accuracy = sum ( as.character(obs) == as.character(pred) )

  accuracy = accuracy / sum(tt)

  error.rate = 1 - accuracy

  ## Output
  list( Accuracy = accuracy, Error.rate = error.rate )
}


