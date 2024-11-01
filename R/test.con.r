
#' @title t-test and Wilcoxon test
#' @description Calculation mean and SD in 2 populations, with t-test or Wilcoxon test
#' @param x Explanatory quantitative variable
#' @param evento Response qualitative variable with 2 categories
#' @returns A data.frame with number, mean and SD in 2 populations, p-value and method t-test or Wilcoxon test
#' @examples
#' x <- rnorm(100,0,1)
#' y <- rbinom(100,1,0.5)
#' y <- factor(y, levels=c(0,1), labels = c('low','High'))
#' test.con(x,y)
#' @export
test.con <- function ( x, evento ){
  # Put the variables in order: (explanatory, response)

  #library(nortest)

  if(nlevels(evento) > 2) {
    stop("Error: The response variable has more than 2 categories")
  } else {

    # Checking normality and equality of variances --------------
    p.normal <- nortest::lillie.test(x)$p.value
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
