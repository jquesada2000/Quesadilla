

#' @title AUC area
#' @description Function that calculates the area under the ROC curve, from a binary classification variable and a quantitative variable
#' @param bin.var binary classification variable
#' @param prob.var quantitative variable that can be a probability, total questionnaire score,..
#' @returns A list with AUC and 95% CI
#' @examples
#' prob <- sample(seq(0,1,by=0.0001), 200 , replace=T)
#' resp <- sample(seq(0,1), 200 , replace=T)
#' ROCarea(resp,prob)
#' @export
ROCarea = function (bin.var, prob.var) {

  if ( length( unique (bin.var ) ) != 2 )
  { print ( bin.var ) ; stop ( "The variable must be binary" ) }


  ## AUC con IC95%
  roc.out <- roc( bin.var , prob.var)
  w.auc  = pROC::auc (roc.out)
  CI <- pROC::ci(roc.out)

  ## Output
  list ( AUC = round(w.auc[1],3) , CI95 = paste("(",round(CI[1],3),",",round(CI[3],3),")"))
}
