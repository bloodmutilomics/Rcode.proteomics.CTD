#' Decision curve analysis evaluates the net benefit of a model or test: UVA/UChicago
#'
#' @name decisioCurve.singleSample
#' @author Yong Huang
      
library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)
library(scales)

library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)

######## Read clinical traits of UVA/UChicago cohort

df<- read.csv("data/test.cmb.csv")
row.names(df) <- df[, 1]


Diagnosis <- df$Dx2
Age<-rescale( df$Age_draw )
Sex<-df$sex.score

FVC <- rescale( df$fvc.score )
DLCO <- rescale( df$dlco.score )


GAP.score <- rescale ( df$gap.score0 )
GAP.stage<- rescale( df$gap.stage )

SVM  <- rescale(  (df$SVM.decision) )
LASSO <- rescale( ( df$LASSO.link) )
RF <- rescale(  df$RF.prob )
Imbalance.RF <- rescale( df$Imbalanced.RF.prob )
Composite_Class<- df$CDS

###### decision cuvve of clinical traits
dca_ml <- dca(Dx ~ sex.score + Age + FVC + DLCO,  df) %>% plot(smooth = TRUE)


###### decision cuvve of sex, age, LImbalanced.RF and Composite Diagnosis Score
dca(Diagnosis ~ sex.score + Age + LASSO + RF,  df) %>% plot(smooth = TRUE)

###### decision cuvve of sex, age, LASSO and RF 
dca(Diagnosis ~ sex.score + Age + LASSO + RF,  df) %>% plot(smooth = TRUE)

###### decision cuvve of sex, age and SVM 
dca( Dx ~ sex.score + Age + SVM ,  df) %>% plot(smooth = TRUE)

