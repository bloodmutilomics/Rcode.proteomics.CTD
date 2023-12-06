#' Recursive Feature Elimination pipeline
#'
#' @name caret.pipe.rfe.s120
#' @author Yong Huang
  

# Load necessary libraries
library(dplyr)
library(caret)
library(mlbench)
library(boot)

# Load data
df <- read.csv("../../npx.pff.ipf.ctd2.csv", check.names=F)
rownames(df) <- df[, 1]

# Subset data
dd.ctd <- subset(df, Diagnosis=="CTD")
dd.ipf <- subset(df, Diagnosis=="IPF")

dd.ipf.f <- subset(dd.ipf, Sex=="female")
dd.ipf.m <- subset(dd.ipf, Sex=="male")

dd.ctd.f <- subset(dd.ctd, Sex=="female")
dd.ctd.m <- subset(dd.ctd, Sex=="male")

# Set seed for reproducibility
set.seed(123)

# Initialize variables
n <- 100
sid <- as.data.frame(matrix(nrow=60*4, ncol=n))
bestsize.counts <- as.data.frame(matrix(nrow=length(prt), ncol=n))
rownames(bestsize.counts) <- prt

# RFE loop
for (i in 1:n) {
  # Resampling
  dd.ipf.f.resampling <- sample_n(dd.ipf.f, 60)
  dd.ipf.m.resampling <- sample_n(dd.ipf.m, 60)

  dd.ctd.f.resampling <- sample_n(dd.ctd.f, 60)
  dd.ctd.m.resampling <- sample_n(dd.ctd.m, 60)

  dd <- rbind(dd.ctd.f.resampling, dd.ctd.m.resampling, dd.ipf.f.resampling, dd.ipf.m.resampling)

  Sex <- dd$Sex
  Site <- dd$Site
  Diagnosis <- dd$Diagnosis

  y <- as.numeric(Diagnosis=="IPF")
  y.train <- as.factor(y)

  x.train <- dd[, -c(1:10)]

  xy <- cbind(y.train, x.train)
  xy <- as.matrix(xy)

  train <- cbind(Diagnosis, Sex, Site, x.train)

  # Collect SampleID of train
  sid[, i] <- rownames(train)

  # Prepare training scheme
  control <- trainControl(method="repeatedcv", number=5, repeats=5)

  # Train model
  model <- train(x.train, y.train, method='rocc', preProcess="scale", trControl=control)

  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  imp<- importance$importance

 ################ Remove Redundant Features
  imp.top <- subset(imp, X1>0.6)
  x.train.top<-x.train[, rownames(imp.top)]


  # calculate correlation matrix
  correlationMatrix <- cor(x.train.top)
  #write.csv(correlationMatrix, file="correlationMatrix.csv")

  # summarize the correlation matrix
  # print(correlationMatrix)

  # find attributes that are highly corrected (ideally > 0.7)
  highlyCorrelated <- findCorrelation(correlationMatrix, exact=T, cutoff=0.70)

  # remove highlyCorrelated features 
  x.train.top.flt <-x.train.top[, -c(highlyCorrelated) ]

  Diagnosis<- y.train
  dat<- cbind(Diagnosis, x.train.top.flt)
  # write.csv(dat, file="imp.top.flt.highlyCorrelated.07.csv")


  # Feature Selection 
  control <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10)

  # run the RFE algorithm. size: the number of features that should be retained
  results <- rfe(dat[, -1], dat[, 1], sizes=c(120:ncol(x.train.top.flt) ), rfeControl=control)   

  # saveRDS(results, file="results.fre.Iter.rds")
  # write.csv(results$pred, file="results.pred.csv")

  rs<- results$results
  # pickSizeBest(rs, metric = "Accuracy", maximize = T)

  # pickSizeTolerance(rs, metric="Accuracy", tol = 1.5, maximize=T)

  # summarize the results
  #print(results)

  bestsize <- results$optVariables
  # write.csv(bestsize, file="bestsize.csv")

  match<- match (prt, bestsize)
  bestsize.counts[, i] <- match

} #End of  for{}  loo


########## save RFE output
write.csv(bestsize.counts, file="bestsize.counts0.csv") 

## convert NA to 0
xx<- bestsize.counts

xx[is.na(xx)]<- 0
xx[xx>0]<- 1
Sum<- rowSums(xx)
write.csv(cbind(Sum, xx), file="bestsize.counts1.csv")


### colname =resampling
cname<-""
for (i in 1:ncol(sid) ) {
   cname[i] <- paste("Resampling", i, sep="") 
}

colnames(sid) <- cname
colnames(bestsize.counts) <- cname


## remove duplicate resampling Train cohort 
library(digest)
sid2<- sid[!duplicated(lapply(sid, digest))]
bestsize.counts2<- bestsize.counts[, colnames(sid2)]
write.csv(bestsize.counts2, file="bestsize.counts.csv")

############### Formating output ###############
df<-bestsize.counts
rownames(df) <-df[, 1]
dd<-df[, -1]

dd[is.na(dd)] <-0
dd[dd>0]<- 1

Sum<- rowSums(dd)
write.csv(cbind(Sum, dd), file="bestsize.counts2.csv")






