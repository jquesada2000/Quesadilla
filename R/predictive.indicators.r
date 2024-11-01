

  #' @title Predictive indicators
  #' @description Function that calculates predictive indicators S, E, Youden, PPV, PNV, LR+ and LR- with their 95% CI.
  #' @description 95% CIs are based on the normal binomial approximation. This approximation is ood if the True Positive
  #' @description and False Negative are greater than 5, and the patients come from a same sample.
  #' @param prob probability of response
  #' @param resp membership class
  #' @param cutoff cutoff of probability
  #' @returns A list of indicators
  #' @examples
  #' prob <- sample(seq(0,1,by=0.0001), 200 , replace=T)
  #' resp <- sample(seq(0,1), 200 , replace=T)
  #' cutoff <- seq(0,1,by=0.05)
  #' predictive.indicators(prob,resp,cutoff)
  #' @export
predictive.indicators <- function (prob, resp, cutoff){

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
