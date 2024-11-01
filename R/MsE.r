


  #' @title Mean Square Error
  #' @description Calculates the MSE of an estimated quantitative variable over an observed quantitative variable.
  #' @param cont.obs Data vector of observed quantitative variable
  #' @param cont.pred Data vector of estimated quantitative variable
  #' @returns A list with MSE and Error
  #' @examples
  #' x <- rnorm(100,0,1)
  #' y <- rnorm(100,0,1)
  #' MsE(x,y)
  #' @export
MsE = function ( cont.obs, cont.pred )
{

  if ( length(cont.obs) != length(cont.pred) )
  { stop ( "The variables must have same length" ) }

  ## MSE
  mse = mean ( ( cont.obs - cont.pred ) ** 2 )


  ## Standard error
  error.mse = sd ( ( cont.obs - cont.pred ) ** 2 ) / sqrt(length(cont.obs))


  ## Output
  list( MSE = mse, SE.MSE = error.mse )

}
