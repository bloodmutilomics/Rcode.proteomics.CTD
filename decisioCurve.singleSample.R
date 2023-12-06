#' Decision curve analysis evaluates the net benefit of a model or test: RECEITAL/UC-Davis
#'
#' @name decisioCurve.singleSample
#' @author Yong Huang
      
library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)
library(scales)

library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)

# Read classification of each model and composite diagnosis score
df<- read.csv("TableE6B.CDS.csv")
row.names(df) <- df[, 1]

Diagnosis <- df$Diagnosis.code
Age<-rescale( df$Age0)
Sex<- df$Sex


SVM  <- rescale(  (df$median.pclass.svm) )
LASSO <- rescale( ( df$median.pclass.lasso))
RF <- rescale(  df$median.pclass )
Imbalance.RF <- rescale( df$imbalance.rf)
Composite_Class <- df$CDS

#################### decision curves combined
dca(Diagnosis ~ Sex + Age+ SVM + LASSO + RF + Imbalance.RF + Composite_Class,  df) %>% plot(smooth = TRUE)



