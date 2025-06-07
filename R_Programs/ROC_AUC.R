##In the below code , we simulated the data to classify obese and not obese
##We fit the data on ROC curve and get the AUCs
## We fit on Logistic Regression and Random Forest and compare the AUCs to get a better idea of which model classfies more accurately




library(pROC) # install with install.packages("pROC")
library(randomForest) # install with install.packages("randomForest")

set.seed(42) ##initializes random number generater, used for reproducability later

num.samples<-100

weight<-sort(rnorm(n=num.samples,mean=172,sd=29)) ## Generates 100 samples with mean of 172 lbs and sd of 29 lbs and sorts them and stores in weight

obese<- ifelse(test=(runif(n=num.samples)<(rank(weight)/100)),yes=1,no=0)
## runif(n=num.samples) acts as a threshold. generates random 100 numbers between 0 to  1
## rank(weight)  ranks the weights (assigns 1 to the lowest and 100 to the highest). 
##if the current weight is greater than the current threshold,  obese or else not obese

obese

plot(x=weight,y=obese)


glm.fit=glm(obese~weight,family=binomial)
lines(weight,glm.fit$fitted.values)

### glm.fit$fitted.values contains y axis coordinates, or estimated probabilities that each sample is obese

roc(obese,glm.fit$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,xlab="False Postitive Rate",
ylab="True Positive Rate",lwd=4)
## obese is actual
## glm.fit$fitted.values gives an idea of the predicted


roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes=TRUE) ## contains the roc data information

roc.df<-data.frame(tpp=roc.info$sensitivities*100,fpp=(1-roc.info$specificities)*100,thresholds=roc.info$thresholds)

head(roc.df) ## get first 6 rows

tail(roc.df) ##bottom 6 rows

rf.model<-randomForest(factor(obese)~weight)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)

##rf.model$votes[,1] no of trees that voted correctly 


### Logistic Regression AUC > Random Forest AUC , therefore Logistic Regression is better classification Model

