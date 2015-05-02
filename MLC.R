rm(list=ls(all=T))
library(e1071)
setwd("/Volumes/MySpace/Courses/Sem2/CSC791/Finals/final-data")
training.df <- read.csv("ilk-tr-d.csv",header=TRUE)

means <- aggregate(training.df, list(training.df$CL), mean)
means <- means[,-c(1,5)]
training.df.data <- training.df[,-c(4)]
training.df.1 <- subset(training.df, CL == 1)
training.df.2 <- subset(training.df, CL == 2)
training.df.3 <- subset(training.df, CL == 3)
training.df.4 <- subset(training.df, CL == 4)
training.df.5 <- subset(training.df, CL == 5)

label1 <- do.call(rbind.data.frame,apply(training.df.1[,1:3],1,'-',means[1,]))
sig.1 <- sum(label1 * label1)/nrow(training.df.1[,1:3])
label2 <- do.call(rbind.data.frame,apply(training.df.2[,1:3],1,'-',means[2,]))
sig.2 <- sum(label2 * label2)/nrow(training.df.2[,1:3])
label3 <- do.call(rbind.data.frame,apply(training.df.3[,1:3],1,'-',means[3,]))
sig.3 <- sum(label3 * label3)/nrow(training.df.3[,1:3])
label4 <- do.call(rbind.data.frame,apply(training.df.4[,1:3],1,'-',means[4,]))
sig.4 <- sum(label4 * label4)/nrow(training.df.4[,1:3])
label5 <- do.call(rbind.data.frame,apply(training.df.5[,1:3],1,'-',means[5,]))
sig.5 <- sum(label5 * label5)/nrow(training.df.5[,1:3])

label1 <- do.call(rbind.data.frame,apply(training.df.1[,1:3],1,'-',means[1,]))
sig.1 <- sum(label1 * label1)/nrow(training.df.1[,1:3])

sig.val = c(sig.1,sig.2,sig.3,sig.4,sig.5)
test.df <- read.csv("ilk-te-d.csv",header=TRUE)
scores <- NULL
##Calculate Score

condProb <- function (data, meanVal,sig,prior){
  for(i in 1:5) {
    sc1 <- do.call(rbind.data.frame,apply(data[,1:3],1,'-',meanVal[i,]))
    s1 <- as.data.frame(rowSums(sc1 * sc1))
    s2 <- (1/sig[i])
    g1 <- (-1/2)*s1 * s2 - (1/2)*log(abs(sig[i])) + log(prior[i])
    if (i == 1 ){
      scores <- g1
    } else {
      scores <- cbind(scores, g1)
    }
  }
  return(scores)
}

###################
####Equal Priors###
###################

k <- 1/5
priors <- rep(k,5)
scoreEqual <- condProb(test.df,means,sig.val,priors)
results <- (apply(scoreEqual,1,which.max))
confusionMatrix(results,test.df[,4])

###################
####Naive Bayes####
###################

training.df$CL  <- as.factor(training.df$CL)
naiveModel <- naiveBayes(CL ~ ., data = training.df)
test.df$CL  <- as.factor(test.df$CL)
confusionMatrix(predict(naiveModel, test.df[,-c(4)]), test.df$CL)

######################
###Non Equal Priors###
######################

priors <- NULL
for(i in 1:5){
  training.df.subset <- subset(training.df, CL == i)
  count  <- nrow(training.df.subset)/nrow(training.df)
  priors <- c(priors,count)
}
scoreNotEqual <- condProb(test.df,means,sig.val,priors)
results <- (apply(scoreNotEqual,1,which.max))
confusionMatrix(results,test.df[,4])

