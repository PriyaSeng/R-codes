install.packages("ggthemes")
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
install.packages("party")
library(party)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)


customer <- read.csv("CustomerChurn.csv")

head(customer)

str(customer)

#Finding the missing values
#sapply(customer, function(x) sum(is.na(x)))
options(repr.plot.width = 6, repr.plot.height = 4)
missingdata <- customer %>% summarise_all(funs(sum(is.na(.))/n()))
missingdata <- gather(missingdata, key = "variables", value = "percent_missing")
ggplot(missingdata, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

#There are only 11 missing data in the TotalCharges field, so getting rid of those rows from the dataset.
customer <- customer[complete.cases(customer),]

#Data Cleaning and Exploratory Data Analysis
# Transforming the "No Internet Service" to "No" for six columns which are "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "streamingTV", "streamingMovies".


customer$MultipleLines <- as.factor(mapvalues(customer$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
customer$OnlineSecurity <- as.factor(mapvalues( customer$OnlineSecurity, 
                                            from=c("No internet service"),
                                            to=c("No")))
customer$OnlineBackup <- as.factor(mapvalues( customer$OnlineBackup, 
                                          from=c("No internet service"),
                                          to=c("No")))
customer$DeviceProtection <- as.factor(mapvalues( customer$DeviceProtection, 
                                              from=c("No internet service"),
                                              to=c("No")))
customer$TechSupport <- as.factor(mapvalues( customer$TechSupport, 
                                         from=c("No internet service"),
                                         to=c("No")))
customer$StreamingMovies <- as.factor(mapvalues( customer$StreamingMovies, 
                                             from=c("No internet service"),
                                             to=c("No")))
customer$StreamingTV <- as.factor(mapvalues( customer$StreamingTV, 
                                                 from=c("No internet service"),
                                                 to=c("No")))

#Finding the minimum value and maximum value of the tenure

min(customer$tenure)
max(customer$tenure)

#Since the minimum tenure is 0 month and maximum tenure is 72 months, we can group them into five tenure groups: "0-12 Month", "12-24 Month", "24-48 Months", "48-60 Month", "> 60 Month"
customertenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

customer$grouptenure <- sapply(customer$tenure,customertenure)
  customer$grouptenure <- as.factor(customer$grouptenure)
  
  # Transforming the values in Column "Seniorcitizen" from 0 or 1 to "No" or "Yes".
  
  customer$SeniorCitizen <- as.factor(mapvalues(customer$SeniorCitizen,
                                             from=c("0","1"),
                                             to=c("No", "Yes")))
  
  # Excluding the unnecessary columns which are CustomerID and tenure
  customer <- customer[, -c(1,6)]
  
  #Plotting the correlation plot for numeric variables
  varnum<-sapply(customer,is.numeric)
matrixnum <- cor(customer[,varnum])
corrplot(matrixnum, main="\n\nCorrelation Plot for Numerical Variables", method="circle")

#The Monthly Charges and Total Charges are correlated. So one of them will be removed from the model. We remove Total Charges.

customer <- customer[,-c(18)]

# Data visualoization of Categorical Variables

theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")
options(repr.plot.width = 12, repr.plot.height = 8)

plot_grid(ggplot(customer, aes(x=gender,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(customer, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(customer, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(customer, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(customer, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(customer, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")
#Churn rate is much higher in case of Fiber Optic InternetServices.
#Customers who do not have services like No OnlineSecurity , OnlineBackup and TechSupport have left the platform in the past month.


a1 <- ggplot(customer, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5,fill= StreamingMovies) + ylab("Percentage") + coord_flip() + theme_minimal()
a2 <- ggplot(customer, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..),fill= Contract), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
a3 <- ggplot(customer, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..),fill= PaperlessBilling), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
a4 <- ggplot(customer, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..),fill=PaymentMethod), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
a5 <- ggplot(customer, aes(x=grouptenure)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..),fill=grouptenure), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(a2, a3, a4, a5, ncol=2,top="Churn distribution")

#Logistic Regression
#First, we split the data into training and testing sets:

model<- createDataPartition(customer$Churn,p=0.7,list=FALSE)
set.seed(2018)
training<- customer[model,]
testing<- customer[-model,]
#Confirm the splitting is correct:

dim(training); dim(testing)
attach(customer)
LRmodel<- glm(Churn ~ ., family=binomial,data = training)

summary(LRmodel)

library(ROCR)
library(pROC)
trainpred <- predict.glm(LRmodel, newdata=training,type="response")
predROC <- prediction(trainpred, training$Churn)
perfROC <- performance(predROC,'tpr','fpr')
plot(perfROC,main = "Logistic ROC Curve")  
auc <- performance(predROC, measure="auc")
auc <- auc@y.values[[1]]
auc
area <- format(round(auc, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))


#SVM
library(e1071)
SVMmodel1 <- svm(Churn~.,training)
SVMpredtrain <- predict(SVMmodel1,training)
predSVMROCR <- prediction(as.numeric(SVMpredtrain),as.numeric(training$Churn))
perfSVMROCR <- performance(predSVMROCR,'tpr','fpr')
plot(perfSVMROCR,main = "SVM ROC Curve")
aucSVM <- performance(predSVMROCR,measure="auc")
aucSVM <- aucSVM@y.values[[1]]
aucSVM
area <- format(round(aucSVM, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))

#random forest
library(randomForest)
class(Churn)
modelRF <- randomForest(Churn~., data = training, importance= TRUE, ntree=1000)
summary(modelRF)  
predictions <- as.vector(modelRF$votes[,2])
predictRF <- predictions
ROCRRandom <- prediction(predictRF,training$Churn)
ROCRperfRandom <- performance(ROCRRandom,'tpr','fpr')
plot(ROCRRandom,main = "Random forest ROC Curve")  
aucRandom <- performance(prediction(predictRF,training$Churn),measure='auc')
aucRandom <- aucRandom@y.values[[1]]
aucRandom  
area <- format(round(aucRandom, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))

#decision tree
library(rpart)
treemodel <- rpart(Churn~., training)
plot(treemodel)
text(treemodel, pretty=0)
printcp(treemodel)
plotcp(treemodel)
ptree<- prune(treemodel,cp= treemodel$cptable[which.min(treemodel$cptable[,"xerror"]),"CP"])
plot(ptree, uniform=TRUE, main="Pruned Classification Tree")
text(treemodel, pretty=0)

threshold <- 0.5
predtree <- predict(ptree, testing,type = "vector")>= threshold
predclass <- ifelse(predtree==TRUE,1,0)
print("Confusion Matrix for Decision Tree"); table(Predicted = predclass, Actual = testing$Churn)
p1 <- predict(ptree, training,type = "vector")>= threshold
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = predtree, Actual = testing$Churn)

print(paste('Decision Tree Accuracy on train',sum(diag(tab1))/sum(tab1)))


print(paste('Decision Tree Accuracy on test',sum(diag(tab2))/sum(tab2)))


#Cradient boosting tree
install.packages("gbm")
library(gbm)
GBMmodel1 <- gbm(Churn~., data = training,
                      distribution= "gaussian",n.trees=1000,shrinkage = 0.01,interaction.depth=4)
summary(GBMmodel1)
# the most important factor to predict churn is "Contract" followed by "tenure". 
GBMTrainpred <- predict(GBMmodel1,training,n.trees=1000)
ROCRGBM <- prediction(GBMTrainpred, training$Churn)
ROCRperfGBM <- performance(ROCRGBM,'tpr','fpr')
plot(ROCRGBM,main = "GBM ROC Curve")
aucGBM <- performance(prediction(GBMTrainpred, training$Churn),measure='auc')
aucGBM <- aucGBM@y.values[[1]]
aucGBM
area <- format(round(aucGBM, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))
#performance comparison
m <- matrix(c(auc,aucSVM,aucRandom,aucGBM),nrow=4,ncol=1)
colnames(m) <- c("AUC Value")
rownames(m) <- c("Logistic Regression","SVM","Random Forest","Gradient Boosting")
m
plot(perfROC, col="red",colorsize = TRUE, text.adj = c(-.2,1.7), main="AUC Curves - 4 Models ")
plot(ROCRperfRandom,add=TRUE,col="green", colorsize = TRUE, text.adj = c(-.2,1.7))
plot(perfSVMROCR,add=TRUE,col="blue", colorsize = TRUE, text.adj = c(-.2,1.7))
plot(ROCRperfGBM,add=TRUE,col="black", colorsize = TRUE, text.adj = c(-.2,1.7))
labels <- c("GLM: AUC=83.83%","Random Forest: AUC=81.11%","SVM: AUC=67.16%", "Gradient Boosting: AUC=85.61%")
legend("bottom",xpd=TRUE,inset=c(0,0),labels,bty="n",pch=1,col=c("red","green","blue","black"))

#From all the above AUC Curves method dekivering the most accurate model is Gradient Boosting with a AUC value of 85.61%
GBMmodel1test <- gbm(Churn~., data = testing,
                     distribution= "gaussian",n.trees=1000,shrinkage = 0.01,interaction.depth=4)
summary(GBMmodel1test)
predGBMtest <- predict(GBMmodel1test,testing,n.trees=1000)
GBMROCRpredtest <- prediction(predGBMtest,testing$Churn)
perfROCRTestGBM <- performance(GBMROCRpredtest,'tpr','fpr')
plot(perfROCRTestGBM,main = "GBM ROC Curve ON TEST SET")
aucTestGBM <- performance(GBMROCRpredtest,measure="auc")
aucTestGBM <- aucTestGBM@y.values[[1]]
aucTestGBM
aucGBM
area <- format(round(aucTestGBM, 4), nsmall = 4)
text(x=0.8, y=0.1, labels = paste("AUC =", area))
#conclusion
a <- matrix(c("The Best Model","Average Model","Average Model","Underperforming"),nrow=4,ncol=1)
colnames(a) <- c("General Performance (accuracy) of the algorithms")
rownames(a) <- c("Gradient Boosting","Logistic Regression","Random Forest","Support Vector Machine")
a
