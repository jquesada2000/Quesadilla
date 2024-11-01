

#' @title Anova and Kruskall-Wallis test
#' @description Calculation mean and SD in 3 populations, with Anova or Kruskall-Wallis test
#' @param x Explanatory quantitative variable
#' @param evento Response qualitative variable
#' @param ncat number of categories of response variable (ncat > 2)
#' @returns A data.frame with number, mean and SD in ncat populations, p-value and method Anova or Kruskall-Wallis test
#' @examples
#' x <- rnorm(100,0,1)
#' y <- rbinom(100,2,0.5)
#' y <- factor(y, levels=c(0,1,2), labels = c('low','Med','High'))
#' test.con.plus(x,y,3)
#' @export
test.con.plus <- function ( x, evento, ncat ){
  # Put the variables in order: (explanatory, response, number categories)

  #library(nortest)

  if(ncat == 2) {
    stop("Error: The response variable has 2 categories")
  } else {

    # Checking normality and equality of variances --------------
    p.normal <- nortest::lillie.test(x)$p.value
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
