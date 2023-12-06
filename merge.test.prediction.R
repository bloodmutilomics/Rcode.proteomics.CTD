#' merge class prediction by diverse machine learning 
#'
#' @name merge.test.prediction
#' @author Yong Huang
      

df1<- read.csv("pred.class.svm.csv")
df3<- read.csv("pred.class.rf.csv")
df2<- read.csv("pred.class2.lasso.csv")
df4<-read.csv("qclassifier2.single.class.csv" )

rownames(df1) <- df1[, 1]
rownames(df2) <- df2[, 1]
rownames(df3) <- df3[, 1]
rownames(df4) <- df4[, 1]

##########################
dd1<- df1[, c(1:6)]
dd2<-df2[rownames(df1), c(2:6)]
dd3<-df3[rownames(df1), c(2:6)]
dd4<-df4[rownames(df1), c(2:7)]

############
dd<- cbind(dd1, dd2, dd3, dd4)
write.csv(dd, file="pred.class.cmb.csv")