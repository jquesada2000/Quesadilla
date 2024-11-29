## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,  # Anchura de las figuras
  fig.height = 4, # Altura de las figuras
  fig.retina = 2, # Resolución para pantallas de alta densidad
  out.width = "100%" # Tamaño relativo en el documento final
)

## ----eval=FALSE---------------------------------------------------------------
# 
# library(genridge)
# 
# data("prostate")
# 
# 
# names(prostate)
# 
# head(prostate)
# 
# datos <- prostate[,-10]
# names(datos)
# 
# class(datos$svi)
# 
# datos$svi <- as.factor(datos$svi)
# levels(datos$svi) <- c("No", "Si")
# table(datos$svi)
# 
# 
# ## ----------------------------------------------------
# 
# library(AppliedPredictiveModeling)
# 
# data(AlzheimerDisease)
# 
# 
# names(predictors)
# 
# head(predictors)
# 
# diagnosis
# 
# 
# 
# datos0 <- predictors[, 125:129]
# names(datos0)
# 
# datos <- data.frame(diagnosis=diagnosis,datos0)
# head(datos)
# 

## ----eval=FALSE---------------------------------------------------------------
# library(genridge)
# 
# data("prostate")
# 
# help(prostate)
# 
# names(prostate)
# 
# head(prostate)
# 
# datos <- prostate[,-10]
# names(datos)
# 
# class(datos$svi)
# 
# datos$svi <- as.factor(datos$svi)
# levels(datos$svi) <- c("No", "Yes")
# table(datos$svi)
# 
# 
# 
# ### graficos uni ------------------------------------
# library(nortest)
# vari <- names(datos)
# 
# par(mfrow = c(3,3))
# 
# for(j in 1:length(vari)){
# 
# 
#   if(is.factor(datos[,j])) {
# 
#     barplot(table(datos[, j]),ylab="Frencuency", xlab=vari[j], main="")
# 
#   } else {
#     p <- round(lillie.test(datos[, j])$p.value,3)
#     hist(datos[, j], freq=F, ylab="Frencuency", xlab=vari[j], main=paste("Normality p-value = ",p))
# 
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# 
# 
# library(genridge)
# 
# data("prostate")
# 
# names(prostate)
# 
# head(prostate)
# 
# dataset  <- prostate[,-10]
# names(dataset )
# 
# class(dataset $svi)
# 
# dataset$svi <- as.factor(dataset $svi)
# levels(dataset $svi) <- c("No", "Si")
# table(dataset $svi)
# 
# source("funciones.r")
# 
# 
# library(plyr)
# 
# 
# vari <- names(dataset)
# out <- out2 <- NULL
# for( name.var in vari){
# 
#   if(is.factor(dataset[,name.var])) {
#     out[[name.var]] <- descrip.cat(dataset[, name.var])
#   } else {
# 
#     out2[[name.var]] <- descrip.con(dataset[, name.var])
#   }
# }
# out.cat <- ldply (out, data.frame)
# out.con <- ldply (out2, data.frame)
# 
# 
# 
# 
# # We remove repeated names from the variables and reorder the data.frame
# for(i in 2:length(out.cat$.id)){
# 
#   out.cat$var[1] <- out.cat$.id[1]
# 
#   if( out.cat$.id[i] == out.cat$.id[i-1]) {
# 
#     out.cat$var[i] <- ""
# 
#   } else {
# 
#     out.cat$var[i] <- out.cat$.id[i]
# 
#   }
# }
# 
# out.cat2 <- out.cat[,-1]
# out.cat2 <- out.cat2[,c(4,1,2,3)]
# 
# 
# 
# 
# library(officer)
# library(flextable)
# library(magrittr)
# 
# 
# my.table.style = function(x){
#   std_b = fp_border(color="black")
#   cuali.t <- regulartable(x)
#   cuali.t <- set_formatter_type(cuali.t, fmt_double = "%.01f")
#   cuali.t <- fontsize(cuali.t, size=10)
#   cuali.t <- font(cuali.t, fontname = "Calibri")
#   cuali.t <- align(cuali.t, align="left", j=1)
#   cuali.t <- align(cuali.t, align="left", j=2)
#   cuali.t <- border_remove(cuali.t)
#   cuali.t <- hline(cuali.t, border = std_b, part="header")
#   cuali.t <- hline_top(cuali.t, border = std_b, part="all")
#   cuali.t <- hline_bottom(cuali.t, border = std_b, part="all")
#   cuali.t <- width(cuali.t, width = 1.4)
# }
# 
# 
# out.cat.flex <- my.table.style (out.cat2)
# out.con.flex <- my.table.style (out.con)
# 
# 
# my_doc <- read_docx( )%>%
#   body_add_par("Factor variables") %>%
#   body_add_flextable(out.cat.flex)%>%
#   body_add_par("") %>%
#   body_add_break() %>%
#   body_add_par("Cuantitatives variables" ) %>%
#   body_add_flextable(out.con.flex)%>%
#   body_add_par("")
# print(my_doc, target = "descriptives.docx")
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# library(genridge)
# 
# data("prostate")
# 
# help("prostate")
# 
# names(prostate)
# 
# head(prostate)
# 
# datos <- prostate[,-10]
# names(datos)
# 
# class(datos$svi)
# 
# datos$svi <- as.factor(datos$svi)
# levels(datos$svi) <- c("No", "Si")
# table(datos$svi)
# 
# datos$lpsa_exp <- exp(datos$lpsa)
# quantile(datos$lpsa_exp,0.75)
# 
# datos$lpsa_exp_cat2[datos$lpsa_exp < 21.25] <- 0
# datos$lpsa_exp_cat2[datos$lpsa_exp >= 21.25] <- 1
# 
# datos$lpsa_exp_cat2 <- factor(datos$lpsa_exp_cat2, labels=c("<P75",">=P75"))
# 
# table(datos$lpsa_exp_cat2)
# 
# names(datos)
# 
# datos2 <- datos[,-c(9,10)]
# names(datos2)
# 
# source("funciones.r")
# 
# 
# 
# 
# library(plyr)
# 
# ###------------------------------------- response with 2 categories
# # put the name of the response variable -------------------
# resp.name <- "lpsa_exp_cat2"
# 
# # put the correct dataset ---------------------------------
# dataset <- datos2
# 
# evento <- as.factor(dataset [,resp.name])
# 
# va <- names(dataset)
# vari <- va[-which(va==resp.name)]
# 
# out <- out2 <- NULL
# 
# # applying the functions -----------------------------------
# for( name.var in vari){
# 
#   if(!is.factor(dataset [,name.var]) & !is.character(dataset [,name.var])) {
#     out[[name.var]] <- test.con(dataset [, name.var],evento)
#   }
# 
#   if(is.factor(dataset [,name.var]) & !is.character(dataset [,name.var])){
# 
#     out2[[name.var]] <- test.cat(dataset [, name.var],evento)
#   }
# }
# 
# # adjusting variable names -----------------------------------
# for(i in 1:length(out)){
#   out[[i]]$ name <- ""
#   out[[i]]$ name[1] <- names(out)[i]
# }
# 
# for(i in 1:length(out2)){
#   out2[[i]]$ name <- ""
#   out2[[i]]$ name[1] <- names(out2)[i]
# }
# out.con <- do.call(rbind.data.frame, out)
# out.cat <- do.call(rbind.data.frame, out2)
# 
# row.names(out.con) <- NULL
# row.names(out.cat) <- NULL
# 
# # reordering the columns --------------------------------------
# out.con <- out.con[,c(7,1,2,3,4,5,6)]
# out.cat <- out.cat[,c(8,1,2,3,4,5,6,7)]
# 
# # convert to flextable ----------------------------------------
# 
# library(officer)
# library(flextable)
# library(magrittr)
# 
# 
# out.con.flex <- my.table.style(out.con)
# out.cat.flex <- my.table.style(out.cat)
# 
# # saving to a word file ----------------------------------------
# 
# # Creation of mydoc, a mydocx object
# my_doc <- read_docx( )%>%
#   body_add_par("Factor variables") %>%
#   body_add_flextable(out.cat.flex)%>%
#   body_add_par("") %>%
#   body_add_break() %>%
#   body_add_par("Quantitatives variables " ) %>%
#   body_add_flextable(out.con.flex)%>%
#   body_add_par("")
# 
# print(my_doc, target = "bivariate.docx")
# 
# 
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# library(genridge)
# 
# data("prostate")
# 
# #help("prostate")
# 
# names(prostate)
# 
# #head(prostate)
# 
# dataset <- prostate[,-10]
# #names(datos)
# 
# #class(datos$svi)
# 
# dataset$svi <- as.factor(dataset$svi)
# levels(dataset$svi) <- c("No", "Si")
# #table(datos$svi)
# 
# 
# head(dataset)
# table(dataset$gleason)
# 
# dataset$gleason_cat3[dataset$gleason==6] <- 1
# dataset$gleason_cat3[dataset$gleason==7] <- 2
# dataset$gleason_cat3[dataset$gleason > 7] <- 3
# 
# dataset$gleason_cat3 <- factor(dataset$gleason_cat3, labels=c("6","7","8-9"))
# 
# table(dataset$gleason_cat3)
# 
# 
# dataset <- dataset[,-7]
# names(dataset)
# 
# head(dataset)
# 
# source("funciones.r")
# 
# 
# library(plyr)
# 
# ###------------------quantitative response, qualitative explanatory variables
# # put the name of the response variable -------------------
# resp.name <- "lpsa"
# 
# # put the correct dataset ---------------------------------
# dataset <- dataset
# 
# evento <- dataset [,resp.name]
# 
# va <- names(dataset)
# vari <- va[-which(va==resp.name)]
# 
# out <- NULL
# 
# # applying the functions -----------------------------------
# for( name.var in vari){
# 
#   if(is.factor(dataset [,name.var]) & !is.character(dataset [,name.var])){
# 
#   ncat <- length (levels(dataset [,name.var]))
# 
#       if(ncat == 2){
# 
#         out[[name.var]] <- test.con(evento, dataset [, name.var])
#       }
# 
#       if(ncat > 2){
# 
#         out[[name.var]] <- test.con.plus(evento, dataset [, name.var], ncat)
# 
#       }
#   } else {
#     out[[name.var]] <- NULL
#   }
# }
# 
# # adjusting variable names -----------------------------------
# for(i in 1:length(out)){
#   out[[i]]$ name <- ""
#   out[[i]]$ name[1] <- names(out)[i]
# }
# 
# 
# out.con <- do.call(rbind.data.frame, out)
# 
# row.names(out.con) <- NULL
# 
# 
# # reordering the columns --------------------------------------
# out.con <- out.con[,c(7,1,2,3,4,5,6)]
# 
# 
# # convert to flextable ----------------------------------------
# 
# library(officer)
# library(flextable)
# library(magrittr)
# 
# 
# out.con.flex <- my.table.style(out.con)
# 
# 
# # saving to a word file ----------------------------------------
# 
# # Creation of mydoc, a mydocx object
# my_doc <- read_docx( )%>%
#   body_add_par("Factor variables") %>%
#   body_add_flextable(out.con.flex)%>%
#   body_add_par("") %>%
#   body_add_par("")
# 
# print(my_doc, target = "bivariate_respCont_expliCa.docx")
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# 
# library(genridge)
# 
# data("prostate")
# 
# help("prostate")
# 
# names(prostate)
# 
# head(prostate)
# 
# dataset <- prostate[,-10]
# names(dataset)
# 
# class(datos$svi)
# 
# dataset$svi <- as.factor(dataset$svi)
# levels(dataset$svi) <- c("No", "Si")
# table(dataset$svi)
# 
# dataset$lpsa_exp <- exp(dataset$lpsa)
# quantile(dataset$lpsa_exp,c(0.25,0.75))
# 
# dataset$lpsa_exp_cat3[dataset$lpsa_exp < 5.65] <- 0
# dataset$lpsa_exp_cat3[dataset$lpsa_exp >= 5.65 & dataset$lpsa_exp <= 21.25] <- 1
# dataset$lpsa_exp_cat3[dataset$lpsa_exp > 21.25] <- 2
# 
# 
# 
# dataset$lpsa_exp_cat3 <- factor(dataset$lpsa_exp_cat3, labels=c("<P25","P25-P75",">P75"))
# 
# table(dataset$lpsa_exp_cat3)
# 
# names(dataset)
# 
# dataset <- dataset[,-c(9,10)]
# names(dataset)
# 
# source("funciones.r")
# 
# 
# 
# library(plyr)
# 
# ###----------------------------- response with 3 categories
# # put the name of the response variable -------------------
# resp.name <- "lpsa_exp_cat3"
# 
# # put the number of response categories
# ncat <- 3
# 
# # put the correct dataset ---------------------------------
# dataset <- dataset
# 
# evento <- as.factor(dataset[,resp.name])
# 
# va <- names(dataset)
# vari <- va[-which(va==resp.name)]
# 
# out <- out2 <- NULL
# 
# # applying the functions -----------------------------------
# for( name.var in vari){
# 
#   if(!is.factor(dataset[,name.var]) & !is.character(dataset[,name.var])) {
#     out[[name.var]] <- test.con.plus(dataset[, name.var],evento,ncat)
#   }
# 
#   if(is.factor(dataset[,name.var]) & !is.character(dataset[,name.var])){
# 
#     out2[[name.var]] <- test.cat3(dataset[, name.var],evento)
#   }
# }
# 
# # adjusting variable names -----------------------------------
# for(i in 1:length(out)){
#   out[[i]]$ name <- ""
#   out[[i]]$ name[1] <- names(out)[i]
# }
# 
# for(i in 1:length(out2)){
#   out2[[i]]$ name <- ""
#   out2[[i]]$ name[1] <- names(out2)[i]
# }
# 
# out.con <- do.call(rbind.data.frame, out)
# out.cat <- do.call(rbind.data.frame, out2)
# 
# row.names(out.con) <- NULL
# row.names(out.cat) <- NULL
# 
# # reordering the columns --------------------------------------
# out.con <- out.con[,c(7,1,2,3,4,5,6)]
# out.cat <- out.cat[,c(10,1,2,3,4,5,6,7,8,9)]
# 
# 
# 
# # convert to flextable ----------------------------------------
# 
# library(officer)
# library(flextable)
# library(magrittr)
# 
# 
# out.con.flex <- my.table.style(out.con)
# out.cat.flex <- my.table.style(out.cat)
# 
# # saving to a word file ----------------------------------------
# 
# # Creation of mydoc, a mydocx object
# my_doc <- read_docx( )%>%
#   body_add_par("Factor variables") %>%
#   body_add_flextable(out.cat.flex)%>%
#   body_add_par("") %>%
#   body_add_break() %>%
#   body_add_par("Quantitatives variables " ) %>%
#   body_add_flextable(out.con.flex)%>%
#   body_add_par("")
# 
# print(my_doc, target = "bivariate_resp_3cat.docx")
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# library(genridge)
# 
# data("prostate")
# 
# help(prostate)
# 
# names(prostate)
# 
# head(prostate)
# 
# dataset <- prostate[,-10]
# names(dataset)
# 
# class(dataset$svi)
# 
# dataset$svi <- as.factor(dataset$svi)
# levels(dataset$svi) <- c("No", "Yes")
# table(dataset$svi)
# 
# names(dataset)
# 
# 
# ### -------------- scatter plot for cuantitative variables
# 
# resp.name <- "lpsa"
# 
# expli.var <- dataset[,!(names(dataset) %in% resp.name)]
# vari <- names(expli.var)
# resp.var <- dataset[,resp.name]
# 
# par(mfrow = c(3,3))
# 
# for(j in 1:length(vari)){
# 
#   if(!is.factor(expli.var[,j])) {
# 
#     plot(resp.var ~ expli.var[,j],ylab=resp.name, xlab=vari[j], main="")
#     lines(lowess(expli.var[,j],resp.var))
#   }
# }
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# 
# library(genridge)
# 
# data("prostate")
# 
# help(prostate)
# 
# names(prostate)
# 
# head(prostate)
# 
# dataset <- prostate[,-10]
# names(dataset)
# 
# class(dataset$svi)
# 
# dataset$svi <- as.factor(dataset$svi)
# levels(dataset$svi) <- c("No", "Yes")
# table(dataset$svi)
# 
# names(dataset)
# 
# 
# ### -------------- scatter plot for cuantitative variables
# 
# resp.name <- "lpsa"
# 
# expli.var <- dataset[,!(names(dataset) %in% resp.name)]
# vari <- names(expli.var)
# resp.var <- dataset[,resp.name]
# 
# for(j in 1:length(vari)){
# 
#   if(!is.factor(expli.var[,j])) {
#     #win.graph()
#     plot(resp.var ~ expli.var[,j],ylab=resp.name, cex.lab = 2,xlab=vari[j], main="")
#     lines(lowess(expli.var[,j],resp.var))
#   }
# }
# 
# 
# ### correlations for quantitative variables----------------------------------
# library(nortest)
# library(plyr)
# 
# dataset <- dataset
# resp.name <- "lpsa"
# 
# # cor.test does not calculate the confidence interval for Spearman's coefficient.
# # It can be calculated by converting the data to ranges, and applying Pearson's.
# 
# 
# # identify which variables are factors
# aa <- lapply( dataset, is.factor)
# bb <- ldply(aa, data.frame)[,2]
# 
# # removing the factors
# dataset1 <- dataset[,!bb]
# 
# # removing the response
# expli.var <- dataset1[,!(names(dataset1) %in% resp.name)]
# 
# vari <- names(expli.var)
# evento <- dataset[,resp.name]
# 
# # Checking normality for response  --------------
# p.normal.resp <- lillie.test(dataset[,resp.name])$p.value
# 
# pvalue <- var.cor.vec <- CIa.vec <- CIb.vec <- met.vec <- vector()
# 
# # loop for explanatory variables
# for(j in 1:length(vari)){
# 
#   if(is.factor(expli.var[,j])) {p<-var.cor<-CIa<-CIb <- met <- 0}
# 
#   if(!is.factor(expli.var[,j])) {
# 
#     # Checking normality--------------
#     p.normal <- lillie.test(expli.var[,j])$p.value
# 
# 
#     # if the variables are normal ----------@
#     if(p.normal >= 0.05 & p.normal.resp >= 0.05){
# 
#       corr <- cor.test(evento , expli.var[,j],method = "pearson")
# 
#       p <- round(corr$p.value,3)
#       var.cor <- round(corr$estimate,3)
#       CIa <- round(corr$conf.int[1],3)
#       CIb <- round(corr$conf.int[2],3)
#       met <- "Pearson"
# 
#     }
#     # if the variables are no normal ----------@
#     if(p.normal < 0.05 | p.normal.resp < 0.05){
# 
#       corr <- cor.test(evento , expli.var[,j],method = "spearman")
# 
#       p <- round(corr$p.value,3)
#       var.cor <- round(corr$estimate,3)
# 
#       CIa <- round(spearman.test(evento , expli.var[,j])[1],3)
#       CIb <- round(spearman.test(evento , expli.var[,j])[2],3)
#       met <- "Spearman"
#     }
#   }
#   pvalue[j] <- p
#   var.cor.vec[j] <- var.cor
#   CIa.vec[j] <- CIa
#   CIb.vec[j] <- CIb
#   met.vec[j] <- met
# }
# res <- data.frame(var=vari,Corr=var.cor.vec, CI95=paste("(",CIa.vec,"; ",CIb.vec,")",sep=""), pvalue=pvalue, method=met.vec)
# 
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# library(genridge)
# 
# data("prostate")
# 
# #help(prostate)
# 
# names(prostate)
# 
# head(prostate)
# 
# dataset <- prostate[,-10]
# names(dataset)
# 
# class(dataset$svi)
# 
# dataset$svi <- as.factor(dataset$svi)
# levels(dataset$svi) <- c("No", "Yes")
# table(dataset$svi)
# 
# names(dataset)
# 
# 
# 
# ## ------------------saturated model
# 
# fit.sat <- lm(lpsa ~
#                 lcavol +
#                 lweight +
#                 age +
#                 lbph +
#                 svi +
#                 lcp +
#                 gleason +
#                 pgg45
#               , data=dataset)
# 
# # AIC
# 
# drop1(fit.sat,test="Chisq")
# 
# 
# vif(fit.sat)
# 
# 
# 
# fit.step <-  step(fit.sat, direcction = "backward")
# 
# 
# summary(fit.step)
# 
# 
# fit2 <- lm(lpsa ~
#                lcavol +
#                lweight +
#                #age +
#                #lbph +
#                svi
#                #lcp +
#                #gleason +
#                #pgg45
#              , data=dataset)
# 
# 
# drop1(fit2,test="Chisq")
# 
# summary(fit2)
# 
# coefficients(fit2)
# 
# ## confusion con chest ------------------------------------
# library(chest)
# 
# 
# # list of possibly confounding variables
# vlist<-c("age", "lbph", "lcp", "gleason", "pgg45")
# 
# 
# results_lcavol <- chest_lm(crude = "lpsa ~ lcavol + lweight + svi", xlist = vlist, data = dataset)
# results_lweight <- chest_lm(crude = "lpsa ~ lweight + lcavol + svi", xlist = vlist, data = dataset)
# results_svi <- chest_lm(crude = "lpsa ~ svi + lweight + lcavol", xlist = vlist, data = dataset)
# 
# chest_plot(results_lcavol, zero=NULL)
# chest_plot(results_lweight, zero=NULL)
# chest_plot(results_svi, zero=NULL)
# 
# chest_forest(results_lcavol)
# 
# 
# fit99 <- lm(lpsa ~
#              lcavol +
#              #lweight +
#              age +
#              lbph +
#              #svi
#            lcp
#            #gleason +
#            #pgg45
#            , data=dataset)
# 
# summary(fit99)
# 
# 
# 
# # evaluating confusion ----------------
# 
# 
# fit3 <- lm(lpsa ~
#              lcavol +
#              lweight +
#              age +
#              #lbph +
#              svi
#            #lcp +
#            #gleason +
#            #pgg45
#            , data=dataset)
# 
# 
# coefficients(fit3)
# 
# 
# 
# fit4 <- lm(lpsa ~
#              lcavol +
#              lweight +
#              age +
#              lbph +
#              svi
#            #lcp +
#            #gleason +
#            #pgg45
#            , data=dataset)
# 
# 
# coefficients(fit4)
# 
# 
# 
# fit.final <- lm(lpsa ~
#              lcavol +
#              lweight +
#              age +
#              lbph +
#              svi  +
#            #lcp
#            #gleason
#            pgg45
#            , data=dataset)
# 
# 
# coefficients(fit.final)
# 
# 
# par(mfrow=c(2,2))
# plot(fit.final)
# 
# 
# aa <- summary(fit.final)
# 
# 
# # residuals
# fit.final.res <- residuals(fit.final)
# 
# # residuals studentized
# library(MASS)
# fit.final.res.stud <- stdres(fit.final)
# 
# 
# par(mfrow=c(2,2))
# # lcavol
# plot(fit.final$model$lcavol, fit.final.res.stud, xlab="lcavol",ylab="Residuals Studentized")
# # add lowess curve
# lines(lowess(fit.final$model$lcavol, fit.final.res.stud),col="red")
# 
# 
# # lweight
# plot(fit.final$model$lweight, fit.final.res.stud, xlab="lweight",ylab="Residuals Studentized")
# # add lowess curve
# lines(lowess(fit.final$model$lweight, fit.final.res.stud),col="red")
# 
# # svi
# plot(fit.final$model$svi, fit.final.res.stud, xlab="svi",ylab="Residuals Studentized")
# # add lowess curve
# lines(lowess(fit.final$model$svi, fit.final.res.stud),col="red")
# 
# 
# 
# ## secuencia temporal de residuos: autocorrelacion
# 
# # temporal sequence of residuals----------------
# plot(fit.final.res.stud,type="b",ylab="Residuals Studentized")
# 
# # autocorrelation test
# durbinWatsonTest(fit.final)
# 
# 
# 
# 
# res <- summary(fit.final)
# res
# 
# 
# str(res)
# 
# 
# ddff <- data.frame(res$coefficients)
# labels <- rownames(ddff)
# bethas <- round(ddff$Estimate,3)
# error <- round(ddff$Std..Error,3)
# tt <- round(ddff$t.value,3)
# valorp <- round(ddff$Pr...t..,3)
# 
# res.fit.final <- data.frame(variables=labels,coef=bethas,error= error,Tvalue= tt, pvalue = valorp)
# 
# 
# n <- nobs(fit.final)
# aic <- round(AIC(fit.final),1)
# R2 <- round(res$adj.r.squared,3)*100
# 
# indic <- data.frame(n=n, AIC=aic,R2=R2)
# indic
# 
# 
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# 
# 
# # id - patient ID number
# # ttr - Time in days until relapse
# # relapse - Indicator of relapse (return to smoking)
# # grp - Randomly assigned treatment group with levels combination or patchOnly
# # age - Age in years at time of randomization
# # gender - Female or Male
# # race - black, hispanic, white, or other
# # employment - ft (full-time), pt (part-time), or other
# # yearsSmoking - Number of years the patient had been a smoker
# # levelSmoking - heavy or light
# # ageGroup2 - Age group with levels 21-49 or 50+
# # ageGroup4 - Age group with levels 21-34, 35-49, 50-64, or 65+
# # priorAttempts - The number of prior attempts to quit smoking
# # longestNoSmoke - The longest period of time, in days, that the patient has previously gone without smoking
# 
# library(asaur)
# 
# data("pharmacoSmoking")
# head(pharmacoSmoking)
# 
# names(pharmacoSmoking)
# 
# 
# dataset <- pharmacoSmoking[,-c(1,2,11,12)]
# 
# names(dataset)
# 
# table(dataset$grp)
# 
# 
# 
# ## ------------------saturated model
# 
# fit.sat <- glm(relapse ~
#                 grp +
#                 age +
#                 gender +
#                 race +
#                 employment +
#                 yearsSmoking +
#                 levelSmoking +
#                 priorAttempts +
#                 longestNoSmoke
#                , family=binomial(logit), data=dataset)
# 
# # AIC
# 
# drop1(fit.sat,test="Chisq")
# 
# library(car)
# vif(fit.sat)
# 
# 
# 
# fit.step <-  step(fit.sat, direcction = "backward")
# 
# 
# summary(fit.step)
# 
# 
# fit2 <- glm(relapse ~
#               grp +
#               age
#               #gender +
#               #race +
#               #employment
#               #yearsSmoking +
#               #levelSmoking +
#               #priorAttempts
#               #longestNoSmoke
#             , family=binomial(logit), data=dataset)
# 
# 
# drop1(fit2,test="Chisq")
# 
# summary(fit2)
# 
# coefficients(fit2)
# 
# ## confusion con chest ------------------------------------
# library(chest)
# 
# 
# # list of possibly confounding variables
# vlist<-c("gender", "race", "employment", "yearsSmoking","levelSmoking","priorAttempts","longestNoSmoke")
# 
# 
# results_grp <- chest_glm(crude = "relapse ~ grp + age", xlist = vlist, data = dataset, family = "binomial")
# 
# 
# chest_plot(results_grp)
# 
# 
# chest_forest(results_lcavol)
# 
# 
# fit.final <- glm(relapse ~
#               grp +
#               age
#             #gender +
#             #race +
#             #employment
#             #yearsSmoking +
#             #levelSmoking +
#             #priorAttempts
#             #longestNoSmoke
#             , family=binomial(logit), data=dataset)
# 
# 
# summary(fit.final)
# 
# 
# ## comprobando linealidad de la edad por intervalos
# 
# cutoff <- quantile(dataset$age,seq(0,1,0.2))
# cutoff
# 
# dataset$age_cat <- cut(dataset$age, breaks = cutoff, include.lowest = TRUE)
# 
# 
# 
# fit.lin <- glm(relapse ~
#                    grp +
#                    age_cat
# 
#                  , family=binomial(logit), data=dataset)
# 
# 
# summary(fit.lin)
# 
# beta <- as.numeric(coefficients(fit.lin)[3:6])
# label <- c(1,2,3,4)
# 
# #win.graph()
# plot(label, beta, pch = 19, xaxt = "n", xlab="age group", ylab="Log-OR",ylim=c(-2.5,2))
# axis(1, at = c(1,2,3,4), labels=c("39-46","46-52","52-58","58-86"))
# 
# 
# ## comprobando linealidad de la edad por splines
# 
# library(splines)
# 
# fit.splines <- glm(relapse ~
#                  grp +
#                  age +
#                    ns(age, knots = c(39,46,52,58))
# 
#                , family=binomial(logit), data=dataset)
# 
# 
# summary.glm(fit.splines)
# 
# termplot(fit.splines, se=T, terms=3, ylabs="log-OR")
# 
# 
# beta_grp <- round(coefficients(summary.glm(fit.splines))[2,1],4)
# error_grp <- coefficients(summary.glm(fit.splines))[2,2]
# 
# OR_grp <- round(exp(beta_grp),2)
# CI95 <- c(round(exp(beta_grp - 1.96*error_grp),2), round(exp(beta_grp + 1.96*error_grp),2) )
# 
# OR_grp
# CI95
# 
# 
# # Goodness of fit
# n <- nobs(fit.splines)
# n_resp <- sum(fit.splines$y)
# chi2 <- round(fit.splines$null.deviance - fit.splines$deviance,1)
# gl <- fit.splines$df.null - fit.splines$df.residual
# pchi <- round(1 - pchisq(chi2,gl),3)
# 
# library(pROC)
# 
# rocc <- roc( fit.splines$y,fit.splines$fitted.values)
# area <- round(auc(rocc),4)
# IC1 <- round(ci(rocc),4)
# 
# indic1 <- data.frame(n=n, n_relapse=n_resp, Chi2=chi2, pvalue=pchi,area=area,
#                      CI95=paste("(",IC1[1],"-",IC1[3],")",sep=""))
# 
# 
# 
# # LRT otra forma
# 
# fit.null <- glm(relapse ~ 1, family=binomial(logit), data=dataset)
# 
# library(lmtest)
# 
# 
# #perform likelihood ratio test for differences in models
# lmtest::lrtest(fit.splines, fit.null)
# 
# 
# 
# plot.roc(rocc,xlab="1 - Specificity", ylab="Sensitivity")
# 
# 
# 
# # indicadores predictivos
# 
# cutoff <- seq(0,1,0.05)
# prob <- predict(fit.splines, type="response")
# resp <- fit.splines$y
# 
# 
# pred.ind.1 <- predictive.indicators(prob, resp, cutoff)[[1]]
# pred.ind.2 <- predictive.indicators(prob, resp, cutoff)[[2]]
# 
# 
# min(prob)
# max(prob)
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# 
# library(medicaldata)
# 
# data("covid_testing")
# 
# names(covid_testing)
# 
# dataset0 <- covid_testing[,c(4,5,8,9,10,11,13,16,17)]
# 
# names(dataset0)
# 
# dim(dataset0)
# 
# table(dataset0$demo_group)
# 
# dataset <- dataset0[dataset0$demo_group != "unidentified",]
# dim(dataset)
# 
# dataset$drive_thru_ind <- as.factor(dataset$drive_thru_ind)
# levels(dataset$drive_thru_ind) <- c("No", "Yes")
# 
# dataset$orderset <- as.factor(dataset$orderset)
# levels(dataset$orderset) <- c("No", "Yes")
# 
# 
# dataset$result <- as.factor(dataset$result)
# dataset$gender <- as.factor(dataset$gender)
# dataset$demo_group <- as.factor(dataset$demo_group)
# 
# 
# 
# 
# # Download package tarball from CRAN archive and install Epicalc
# download.file(url = "http://cran.r-project.org/src/contrib/Archive/epicalc/epicalc_2.15.1.0.tar.gz", destfile = "epicalc_2.15.1.0.tar.gz")
# install.packages(pkgs="epicalc_2.15.1.0.tar.gz", type="source", repos=NULL)
# unlink("epicalc_2.15.1.0.tar.gz")
# 
# 
# # https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
# 
# 
# 
# 
# library(nnet)
# 
# 
# 
# 
# 
# ## --------------------------------------------------------------- REGRESION MULTINOMIAL NOMINAL ------------------------------
# 
# 
# dataset$result <- relevel(dataset$result, ref = "negative")
# 
# 
# 
# fit.sat <- multinom(result ~
# 
#                    gender +
#                    pan_day +
#                    demo_group +
#                    age +
#                    drive_thru_ind +
#                    orderset +
#                    col_rec_tat +
#                    rec_ver_tat
# 
#                     , data=dataset)
# 
# 
# summary(fit.sat)
# 
# 
# # calculamos Z score y p-valores
# 
# z <- round(summary(fit.sat)$coefficients/summary(fit.sat)$standard.errors,2)
# p <- (1 - pnorm(abs(z), 0, 1))*2
# 
# coeff <- round(t(coef(fit.sat)),3)
# 
# (result <- data.frame(Coef=coeff, Z=t(z),p=round(t(p),3)))
# 
# 
# 
# fit1 <- multinom(result ~
# 
#                       gender +
#                       #pan_day +
#                       demo_group +
#                       age +
#                       drive_thru_ind +
#                       orderset +
#                       col_rec_tat +
#                       rec_ver_tat
# 
#                     , data=dataset)
# 
# 
# 
# z <- round(summary(fit1)$coefficients/summary(fit1)$standard.errors,2)
# p <- (1 - pnorm(abs(z), 0, 1))*2
# 
# coeff <- round(t(coef(fit1)),3)
# 
# (result <- data.frame(Coef=coeff, Z=t(z),p=round(t(p),3)))
# 
# 
# 
# anova(fit.sat,fit1)
# 
# 
# 
# 
# fit2 <- multinom(result ~
# 
#                    #gender +
#                    #pan_day +
#                    demo_group +
#                    age +
#                    drive_thru_ind +
#                    orderset +
#                    col_rec_tat +
#                    rec_ver_tat
# 
#                  , data=dataset)
# 
# 
# 
# z <- round(summary(fit2)$coefficients/summary(fit2)$standard.errors,2)
# p <- (1 - pnorm(abs(z), 0, 1))*2
# 
# coeff <- round(t(coef(fit2)),3)
# 
# (result <- data.frame(Coef=coeff, Z=t(z),p=round(t(p),3)))
# 
# 
# 
# anova(fit1,fit2)
# 
# 
# fit.final <- fit2
# 
# z <- round(summary(fit.final)$coefficients/summary(fit.final)$standard.errors,2)
# p <- (1 - pnorm(abs(z), 0, 1))*2
# 
# coeff <- round(t(coef(fit.final)),3)
# se <- t(summary(fit.final)$standard.errors)
# 
# OR <- round(exp(coeff),2)
# CIa<-round(exp(coeff-1.96*se),2)
# CIb<-round(exp(coeff+1.96*se),2)
# 
# (result <- data.frame(OR=OR,CI95.inv=paste("(",CIa[,1],"-",CIb[,1],")",sep=""),
#                       CI95.pos=paste("(",CIa[,2],"-",CIb[,2],")",sep=""),p=round(t(p),3)))
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# library(MASS)
# 
# data("Pima.tr")
# data("Pima.te")
# 
# dim(Pima.tr)
# dim(Pima.te)
# 
# Pima <- rbind(Pima.tr,Pima.te)
# 
# dim(Pima)
# 
# names(Pima)
# 
# 
# Pima$diabetes[Pima$glu < 100] <- 1
# Pima$diabetes[Pima$glu >= 100 & Pima$glu <= 126] <- 2
# Pima$diabetes[Pima$glu > 126] <- 3
# 
# Pima$diabetes <- factor(Pima$diabetes, labels=c("Normal","pre-diabetes","diabetes"))
# 
# table(Pima$diabetes)
# 
# 
# 
# Pima$bmi_cat[Pima$bmi < 24.9] <- 1
# Pima$bmi_cat[Pima$bmi >= 25 & Pima$bmi <= 29.9] <- 2
# Pima$bmi_cat[Pima$bmi > 29.9] <- 3
# 
# Pima$bmi_cat <- factor(Pima$bmi_cat, labels=c("Normal","Overweight","Obesity"))
# 
# table(Pima$bmi_cat)
# 
# names(Pima)
# 
# 
# dataset <- Pima[,-c(2,5,6,8)]
# 
# names(dataset)
# 
# 
# 
# 
# 
# ## -------------------------------------------------------------------------------------------- MULTINOMIAL ORDINAL
# # MODELO ACUMULADOS PARA DATOS ORDINALES con vglm
# 
# 
# library(VGAM)
# 
# 
# # with proportionals odds----------------------------------
# 
# fit.sat.prop <- vglm(diabetes ~ npreg + bp + skin + age  + bmi_cat,
#             family=cumulative(parallel=TRUE), data=dataset)
# 
# 
# summary(fit.sat.prop)
# 
# 
# 
# 
# 
# # without porportionals odds---------------------------------
# 
# fit.sat <- vglm(diabetes ~ npreg + bp + skin + age  + bmi_cat,
#                 family=cumulative(parallel=F), data=dataset)
# 
# 
# summary(fit.sat)
# 
# 
# # cheking proportionals odds hypotesis ----------------------
# 
# 
# pchisq(deviance(fit.sat.prop)-deviance(fit.sat),
#        df=df.residual(fit.sat.prop)-df.residual(fit.sat), lower.tail=FALSE)
# 
# 
# 
# lrtest(fit.sat.prop, fit.sat)  # Easier
# 
# 
# ## selectionning variables -------------------------
# 
# fit.sat.prop <- vglm(diabetes ~
# 
#                        npreg +
#                        bp +
#                        skin +
#                        age  +
#                        bmi_cat,
# 
#                      family=cumulative(parallel=TRUE), data=dataset)
# 
# 
# drop1.vglm(fit.sat.prop, test="LRT")
# 
# 
# 
# fit2 <- vglm(diabetes ~
# 
#                        #npreg +
#                        bp +
#                        skin +
#                        age
#                        #bmi_cat
# 
#                      , family=cumulative(parallel=TRUE), data=dataset)
# 
# 
# drop1.vglm(fit2, test="LRT")
# 
# 
# ## confusion
# 
# 
# fit4 <- vglm(diabetes ~
# 
#                npreg +
#                bp +
#                skin +
#                age  +
#                bmi_cat
# 
#              , family=cumulative(parallel=TRUE), data=dataset)
# 
# coefficients(fit3)
# coefficients(fit4)
# 
# 
# fit.final <- vglm(diabetes ~
# 
#                npreg +
#                bp +
#                skin +
#                age  +
#                bmi_cat
# 
#              , family=cumulative(parallel=TRUE), data=dataset)
# 
# 
# summary(fit.final)
# 
# 
# 
# 
# marco <- as.data.frame(summary(fit.final)@coef3)
# lab <- row.names(marco)
# b <- marco[,1]
# error <- marco[,2]
# p <- round(marco[,4],3)
# 
# OR <-  round(exp(b),2)
# CIa <- round(exp(b-1.96*error),2)
# CIb <- round(exp(b+1.96*error),2)
# 
# 
# result <- data.frame(var=lab,betha= round(b,3), OR=OR, CI95=paste("(",CIa,"-",CIb,")",sep=""),pvalue=p)
# result
# 
# 
# 
# pred <- fitted(fit.final)
# head(pred)
# 
# 
# 
# clasif <- vector()
# for(i in 1:nrow(dataset)){
#   clasif[i] <- names(which.max(pred[i,]))
# }
# 
# clasif[clasif=="Normal"] <- 0
# clasif[clasif=="pre-diabetes"] <- 1
# clasif[clasif=="diabetes"] <- 2
# 
# clasif <- as.numeric(clasif)
# 
# clasif2 <- factor(clasif,labels=c("Normal","pre-diabetes","diabetes"))
# 
# dataset <- cbind(dataset,pred=clasif2)
# 
# head(dataset)
# 
# 
# table(dataset$diabetes)
# 
# tt <- table(dataset$pred,dataset$diabetes)
# tt
# 
# (accuracy.rate <- sum(diag(tt)) *100 / sum(tt))
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# 
# library(asaur)
# data("pharmacoSmoking")
# 
# dataset <- pharmacoSmoking
# dim(dataset)
# 
# library(survival)
# 
# km <- survfit(Surv(dataset$ttr,dataset$relapse) ~ 1)
# km
# 
# summary(km)
# 
# plot(km)
# 
# 
# logrank.grp <- survdiff(Surv(dataset$ttr,dataset$relapse) ~ dataset$grp)
# logrank.grp
# 
# km.grp <- survfit(Surv(dataset$ttr,dataset$relapse) ~ dataset$grp)
# 
# plot(km.grp,lty = c(3,2), mark.time = FALSE, ylab = "Survival Probability",
#      xlab = "Time (days)", col=c(1,2), lwd=c(2,2), main="Kaplan-Meier")
# legend( "bottomleft", legend = levels(dataset$grp), lty = c(3,2), lwd=c(2,2),
#         col=c(1,2),cex=1, title = "Group", bty = "n", y.intersp=0.6)
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# library(asaur)
# data("pharmacoSmoking")
# 
# dataset <- pharmacoSmoking
# dim(dataset)
# 
# library(survival)
# library(car)
# 
# names(dataset)
# 
# 
# 
# 
# fit.sat <- coxph( Surv(ttr, relapse) ~
# 
#                  grp +
#                  age +
#                  gender +
#                  race +
#                  employment +
#                  yearsSmoking +
#                  levelSmoking +
#                  priorAttempts +
#                  longestNoSmoke
# 
#                , data = dataset)
# 
# drop1(fit.sat,test="Chisq")
# 
# 
# vif(fit.sat)
# 
# 
# fit1 <- coxph( Surv(ttr, relapse) ~
# 
#                     grp +
#                     age +
#                     #gender +
#                     #race +
#                     employment
#                     #yearsSmoking
#                     #levelSmoking +
#                     #priorAttempts +
#                     #longestNoSmoke
# 
#                   , data = dataset)
# 
# drop1(fit1,test="Chisq")
# 
# 
# summary(fit1)
# 
# 
# ## confusion con chest ------------------------------------
# library(chest)
# 
# 
# # list of possibly confounding variables
# vlist<-c("gender", "race", "yearsSmoking","levelSmoking","priorAttempts","longestNoSmoke")
# 
# 
# results_grp <- chest_cox(crude = "Surv(ttr, relapse) ~ grp + age + employment", xlist = vlist, data = dataset)
# 
# chest_plot(results_grp)
# 
# 
# # --------------------------------------------------------
# 
# cox.zph(fit1)
# 
# 
# 
# 
# plot(survfit(fit1), ylim=c(0, 1), xlab="Days",  ylab="Probability of relapse")
# 
# 
# 
# marco<-data.frame(coefficients(summary(fit1)))
# labels<-rownames(marco)
# bethas<-marco$coef
# error<-marco$se.coef.
# valorp<-marco$Pr...z..
# 
# HR<-round(exp(bethas),3)
# aCI<-round(exp(bethas-1.96*error),3)
# bCI<-round(exp(bethas+1.96*error),3)
# (res_fit1<-data.frame(labels,HR=HR,IC=paste("(",aCI,"-",bCI,")",sep=""), pvalue=round(valorp,3)))
# 
# 
# n1 <- fit1$n
# n_resp <- fit1$nevent
# LRT <- as.numeric(round(summary(fit1)$logtest[1],1))
# gl1 <- as.numeric(round(summary(fit1)$logtest[2],0))
# pchi1 <- as.numeric(round(summary(fit1)$logtest[3],3))
# 
# cindex <- as.numeric(round(summary(fit1)$concordance[1],4))
# SDcindex <- summary(fit1)$concordance[2]
# aCI.cindex <- round(cindex - 1.96*SDcindex,4)
# bCI.cindex <- round(cindex + 1.96*SDcindex,4)
# 
# coxzph <- cox.zph(fit1)
# p.risk <- round(coxzph$table[row.names(coxzph$table) == "GLOBAL",][3],3)
# 
# 
# (indic1 <- data.frame(n=n1, n.event=n_resp, LRT=LRT, pChi=pchi1,C.index=cindex,
#                       IC=paste("(",aCI.cindex,"-",bCI.cindex,")",sep=""),p.risk=p.risk))
# 
# 
# 
# 
# # linealidad de covariables
# # ajustamos modelo nulo
# # plot de residuos nulo con covaraibles
# 
# 
# fit.null <- coxph( Surv(ttr, relapse) ~ 1 , data = dataset)
# res.null <- residuals(fit.null,type="martingale")
# 
# 
# library(car)
# 
# new.dat <- seq(min(dataset$age),max(dataset$age),by=1)
# 
# plx <- predict(loess(res.null ~ dataset$age), se=T, newdata = new.dat)
# 
# 
# plot(res.null ~ dataset$age, main="Martingale residuals vs age")
# lines(plx$fit ~ new.dat, lty=1)
# lines(new.dat,plx$fit - qt(0.975,plx$df)*plx$se, lty=2)
# lines(new.dat,plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
# abline(h=0,lty=3)
# 
# 
# # adding splines
# 
# 
# fit.spline <- coxph( Surv(ttr, relapse) ~
# 
#                  grp +
#                  employment  +
#                  pspline(age,5)
# 
#                  , data = dataset)
# 
# drop1(fit.spline,test="Chisq")
# 
# 
# summary(fit.spline)
# 
# 

