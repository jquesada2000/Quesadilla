
my.table.style = function(x){
  std_b = fp_border(color="black")
  cuali.t <- regulartable(x)
  cuali.t <- set_formatter_type(cuali.t, fmt_double = "%.01f")
  cuali.t <- fontsize(cuali.t, size=10)
  cuali.t <- font(cuali.t, fontname = "Calibri")
  cuali.t <- align(cuali.t, align="left", j=1)
  cuali.t <- align(cuali.t, align="left", j=2)
  cuali.t <- border_remove(cuali.t)
  cuali.t <- hline(cuali.t, border = std_b, part="header")
  cuali.t <- hline_top(cuali.t, border = std_b, part="all")
  cuali.t <- hline_bottom(cuali.t, border = std_b, part="all")
  cuali.t <- width(cuali.t, width = 1.4)
}



## --------function for descriptive qualitative explanatory variables
descrip.cat <- function ( var ){
  t1 = table ( var )
  categ <- names(t1)
  p1 = prop.table( t1 )
  porc = round ( 100 * p1, dig = 1 ) 
  data.frame (cat = categ, n = as.vector(t1),p = as.vector(porc) )
}


## --------function for descriptive quantitative explanatory variables
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


spearman.test <- function(x, y, conf.level = 0.95) {
  RIN <- function(x){qnorm((rank(x) - 0.5)/(length(rank(x))))}
  x_rin <- RIN(x)
  y_rin <- RIN(y)
  c('RIN corrected CI'= cor.test(x_rin,y_rin)$conf.int[1:2])
}




###------------------------ response 2cat- function for Chi-Square test, Fisher test
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




###------------------------response 3cat- function for Chi-Square test, Fisher test
test.cat3 <- function ( x, resp ){
  
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
  
  An <- paste("n",colnames(t1)[1],sep="")
  Ap <- paste("%",colnames(t1)[1],sep="")
  
  Bn <- paste("n",colnames(t1)[2],sep="")
  Bp <- paste("%",colnames(t1)[2],sep="")
  
  Cn <- paste("n",colnames(t1)[3],sep="")
  Cp <- paste("%",colnames(t1)[3],sep="")
  
  aa <- data.frame ( names(t1[,1]),t1[,1],p1[,1],t1[,2],p1[,2],t1[,3],p1[,3] ,pvalor2[,1],pvalor2[,2] )
  
  colnames(aa) <- c("cat",An,Ap,Bn,Bp,Cn,Cp,"p-value","method")
  
  
  # removing empty rows -------------
  aa[is.na(aa)] <- ""
  aa
}





###----------------------------- function for t-test, Wilcoxon test
test.con <- function ( x, evento ){
  # Put the variables in order: (explanatory, response)
  
  library(nortest)
  
  if(nlevels(evento) > 2) {
    stop("Error: The response variable has more than 2 categories")
  } else {
    
    # Checking normality and equality of variances --------------
    p.normal <- lillie.test(x)$p.value
    p.varianzas <- var.test(x ~ evento)$p.value
    
    df = data.frame(x,evento)
    dfc = df[complete.cases(df),]
    
    # calculating the valid sample sizes in each group -------
    n1 <- nrow(dfc[dfc$evento == levels(evento)[1],])
    n2 <- nrow(dfc[dfc$evento == levels(evento)[2],])
    
    # if the variable is normal ----------@
    if(p.normal >= 0.05){
      
      if(p.varianzas < 0.05)p.medias <- t.test(x ~ evento)$p.value
      if(p.varianzas >= 0.05) p.medias <- t.test(x ~ evento, var.equal = T)$p.value
      met <- "Student"
      
    }
    # if the variable is not normal ----------@
    if(p.normal < 0.05){
      p.medias <- wilcox.test(x ~ evento)$p.value
      met <- "Wilcoxon"
    }
    # Outcomes ----------------------------------------------
    pvalor <- round(p.medias,3)
    pvalor1 <- as.character(pvalor)
    if(pvalor < 0.001) pvalor1 <- "<0.001"
    
    pvalor2 <- matrix("",nlevels(evento),2)
    pvalor2[1,1] <- pvalor1
    pvalor2[1,2] <- met
    
    medias <- tapply(x, evento, mean, na.rm=T)
    DE <- tapply(x, evento, sd, na.rm=T)
    
    medias <- round(medias,2)
    DE <- round(DE,2)
    
    n <- c(n1,n2)
    names(medias) <- NULL
    names(DE) <- NULL
    
    aa <- data.frame ( cat=levels(evento),n=n,Mean = medias, SD = DE,pvalue=pvalor2[,1],method=pvalor2[,2] )
    aa
  }
}




# ----------------------------- function anova, Kruskall-Wallis test
test.con.plus <- function ( x, evento, ncat ){
  # Put the variables in order: (explanatory, response, number categories)
  
  library(nortest)
  
  if(ncat == 2) {
    stop("Error: The response variable has 2 categories")
  } else {
    
    # Checking normality and equality of variances --------------
    p.normal <- lillie.test(x)$p.value
    p.varianzas <- bartlett.test(x ~ evento)$p.value
    
    
    df = data.frame(x,evento)
    dfc = df[complete.cases(df),]
    
    # calculating the valid sample sizes in each group -------
    n <- vector()
    for(i in 1:ncat){
      n[i] <- nrow(dfc[dfc$evento == levels(evento)[i],])
    }
    # if the variable is normal ----------@
    if(p.normal >= 0.05){
      
      if(p.varianzas < 0.05)p.medias <- oneway.test(x ~ evento)$p.value
      if(p.varianzas >= 0.05) p.medias <- oneway.test(x ~ evento, var.equal = TRUE)$p.value
      met <- "Anova"
    }
    # if the variable is not normal ----------@
    if(p.normal < 0.05){
      p.medias <- kruskal.test(x ~ evento)$p.value
      met <- "Kruskal-W"
    }
    # Outcomes ----------------------------------------------
    pvalor <- round(p.medias,3)
    pvalor1 <- as.character(pvalor)
    if(pvalor < 0.001) pvalor1 <- "<0.001"
    
    pvalor2 <- matrix("",ncat,2)
    pvalor2[1,1] <- pvalor1
    pvalor2[1,2] <- met
    
    medias <- tapply(x, evento, mean, na.rm=T)
    DE <- tapply(x, evento, sd, na.rm=T)
    
    medias <- round(medias,2)
    DE <- round(DE,2)
    
    names(medias) <- NULL
    names(DE) <- NULL
    
    aa <- data.frame ( cat=levels(evento),n=n,Mean = medias, SD = DE,pvalue=pvalor2[,1],method=pvalor2[,2] )
    aa
  }
}





predictive.indicators <- function (prob, resp, cutoff){
  
  library (plyr)
  
  # Function that calculates predictive indicators S, E, PPV, PNV, LR+ and LR- with their 95% CI.
  # Probability(prob), membership class(resp) and probability cutoffs(cutoff) are needed.
  # 95% CIs are based on the normal binomial approximation. This approximation is
  # good if the True Positive and False Negative are greater than 5, and the patients come from a same sample.
  
  
  niter = length(prob)
  
  
  prediction <- vector()
  res0 <- res1 <- NULL
  #corte <- seq(0.5,20,0.5)
  
  for(k in 1:length(cutoff)){
    
    for (i in 1:niter) {
      
      if(prob[i] < cutoff[k]) prediction[i] <- 0
      if(prob[i] >= cutoff[k]) prediction[i] <- 1
      
      
      
    }
    
    resp <- as.factor(resp)
    prediction <- factor(prediction,levels=c(0,1))
    
    tabla <- table(prediction, resp)
    
    # true positive
    a <- tabla[2,2]
    
    # true negative
    d <- tabla[1,1]
    
    # false positive
    b <- tabla[2,1]
    
    # false negative
    c <- tabla[1,2]
    
    # Sensibility
    S <- round( a / (a+c) ,3)
    ICSa <- round( S - 1.96*sqrt(S*(1-S)/(a+c)) ,3)
    ICSb <- round( S + 1.96*sqrt(S*(1-S)/(a+c)) ,3)
    
    # Specificity
    E <- round( d / (b+d) ,3)
    ICEa <- round( E - 1.96*sqrt(E*(1-E)/(b+d)) ,3)
    ICEb <- round( E + 1.96*sqrt(E*(1-E)/(b+d)) ,3)
    
    # Youden index
    Youden <- S + E - 1
    
    # Predictive positive value PPV
    PPV <- round( a / (a+b) ,3)
    ICPPVa <- round( PPV - 1.96*sqrt(PPV*(1-PPV)/(a+b)) ,3)
    ICPPVb <- round( PPV + 1.96*sqrt(PPV*(1-PPV)/(a+b)) ,3)
    
    # Predictive negative value PNV
    PNV <- round( d / (c+d) ,3)
    ICPNVa <- round( PNV - 1.96*sqrt(PNV*(1-PNV)/(c+d)) ,3)
    ICPNVb <- round( PNV + 1.96*sqrt(PNV*(1-PNV)/(c+d)) ,3)
    
    # LR +
    LRP <- round( S / (1-E) ,2)
    ICLRPa <- round( exp( log(LRP) - 1.96*sqrt( (1-S)/a + E/b ) )  ,2)
    ICLRPb <- round( exp( log(LRP) + 1.96*sqrt( (1-S)/a + E/b ) )  ,2)
    
    # LR -
    LRN <- round( (1-S) / E ,2)
    ICLRNa <- round( exp( log(LRN) - 1.96*sqrt( S/c + (1-E)/d ) )  ,2)
    ICLRNb <- round( exp( log(LRN) + 1.96*sqrt( S/c + (1-E)/d ) )  ,2)
    
    
    res0[[k]] <- list(Cutoff=cutoff[k],Se=S, CISe=paste("(",ICSa,"-",ICSb,")",sep=""),
                      Sp=E, CISp=paste("(",ICEa,"-",ICEb,")",sep=""),
                      Youden=Youden)
    
    
    
    res1[[k]] <- list(cutoff=cutoff[k],
                      PPV=PPV, ICppv=paste("(",ICPPVa,"-",ICPPVb,")",sep=""),
                      PNV=PNV, ICpnv=paste("(",ICPNVa,"-",ICPNVb,")",sep=""),
                      LRP=LRP, IClrp=paste("(",ICLRPa,"-",ICLRPb,")",sep=""),
                      LRN=LRN, IClrn=paste("(",ICLRNa,"-",ICLRNb,")",sep="") )
    
    
    
  }
  
  res0.df <- ldply (res0, data.frame, .id=NULL)
  res1.df <- ldply (res1, data.frame, .id=NULL)
  
  return (list(res0.df, res1.df))
  
}





# ---------------------------------------------------- ROC area

ROCarea = function (bin.var, prob.var) {
  
  library(pROC)
  
  if ( length( unique (bin.var ) ) != 2 ) 
  { print ( bin.var ) ; stop ( "The variable must be binary" ) }   
  
  
  ## AUC con IC95%
  roc.out <- roc( bin.var , prob.var)
  w.auc  = pROC::auc (roc.out) 
  
  ## Output
  list ( AUC = w.auc[1] ) 
}


## ------------------------------------------------ error rate

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


## ------------------------------------------------------------------ MSE

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









