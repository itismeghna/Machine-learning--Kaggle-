# Library required for assignment.
library(dplyr)
library(tidyverse)
library(caret)
library(lattice)
library(naivebayes)
library(e1071)
library(pROC)
library(randomForest)
library(ROCR)

#Pulling the dataset for the given Assignment.-----------------------1
heart_disease_Class <- read_csv("heart_disease_modified.csv")
View(heart_disease_Class)

#identifying the datatype of each variable.-----------------------2
str(heart_disease_Class)

#Processing the dataset---------------------------------3
#Hardcoding the character variable to numeric Pace-maker, Drugs, Farm-mist.

heart_disease_Class1 <- heart_disease_Class %>%
mutate(pace_maker=ifelse(pace_maker== "NO", 1,NA))%>%
mutate(drug=ifelse(drug == "Aspirin", 1,(ifelse(drug == "Clopidogrel",2,
ifelse(drug=="Both",3,ifelse(drug=="None",4,NA)))))) %>% 
mutate(fam_hist=ifelse(fam_hist=="yes",1, (ifelse(fam_hist=='no',2,NA))))

# removing X1 identifier in order to blind the subject---------------------------4
heart_disease_Class1<- heart_disease_Class1[ , !(names(heart_disease_Class1) %in% c("X1","Patient_ID"))]

##identifying the datatype of each variable, post hardcoding----------------5
str(heart_disease_Class1)

#summary function to identifying the missing values.-------------------6
summary(heart_disease_Class1)

# counting the number of heart patient and non heart patient.--------------------7
count(heart_disease_Class1, class)

#splitting the data into test and training-----------------8
Data_split<- floor((nrow(heart_disease_Class1)/4)*3)

#randomizing the data-----------------------9
heart_disease_Class2<- heart_disease_Class1[sample(nrow(heart_disease_Class1)),]

#Training data -------------------------10
heart_disease_train <- heart_disease_Class2[1:Data_split,]

# Test Data----------------------11
heart_disease_test <- heart_disease_Class2[(Data_split+1):nrow(heart_disease_Class2), ]
count (heart_disease_test,class)

#train naive Bayes classifier with training data----------------------------12
nb_res <- naiveBayes(class ~ ., data = heart_disease_train)

#predict for test data using the model---------------------------------13
nb_predict <- predict(nb_res, heart_disease_test)

#confusion matrix to determine true positive and true 
#negative based on the selected model.-------------14

ConMatrix <-table(predicted=nb_predict,observed=heart_disease_test$class)
ConMatrix 

#Calculating the Accuracy percentage  from confusion matrix-------------------15

Accuracy <- sum(diag(ConMatrix))/sum(ConMatrix)*100
Accuracy

#Calculating the percentage of missclassification.----------------------------16

Missclassification <- 100-sum(diag(ConMatrix))/sum(ConMatrix)*100
Missclassification

#Calculating the probabilities of the outcome.------------------------------18
Predctprob <- predict(nb_res,heart_disease_test,type="class")
head(Predctprob)
#evaluating the performance-------------------------19
Predctprob1 <- as.numeric(Predctprob)
graphpredict <- prediction(Predctprob1, heart_disease_test$class)
graphpredict


# plotting the ROC curve for naive Bayes ---------------------------20
graphpredict1 <-performance(graphpredict,"tpr","fpr")
plot(graphpredict1,colorize=T,main="ROC",
ylab="Sensitivity",
xlab="1-Sensitivity")
abline(a=0,b=1)

# Determining area under the curve for naive Bayes -------------------------21
auc <- performance(graphpredict,"auc")
auc<-unlist(slot(auc,"y.values"))
auc1<-round(auc,4)
legend(0.6,0.6,auc1,title="AUC",cex=1)



#Random Forest ------------------------22
rffit <- randomForest(as.factor(class)~.,importance=TRUE,data=heart_disease_train)
rffit
summary(rffit)
varImpPlot(rffit)

# Prediction of Data.------------------------23
forest_predict <- predict(rffit, heart_disease_test)
forest_predict

#Confusion Matrix for Random Forest-------------------24
ConMatrix <-table(predicted=forest_predict,observed=heart_disease_test$class)
ConMatrix 
Accuracy_rdm <- sum(diag(ConMatrix))/sum(ConMatrix)*100
Accuracy_rdm

# ROC curve for Random Forest-----------------25
forest_predict1 <- as.numeric(predict(rffit, heart_disease_test, type='class'))
plot(roc(heart_disease_test$class,forest_predict1),main="ROC Forest")

#AUC of Random Forest-----------------26
AUCforest1 <-auc(roc(heart_disease_test$class,forest_predict1),main="ROC Forest")
auc1<-round(AUCforest1,4)
legend(0.2,0.6,auc1,title="AUC",cex=1)


#Q2. Now choose one classifier to further optimize.------------27 

# determining the OOB for the data., it is been observed that error rate cannot be reduced 
#post trees =500
plot(rffit)

# tunning our random forest model----------------28
## tune the model
t_tunning <- tuneRF(heart_disease_train[,-20],as.factor(heart_disease_train$class), stepFactor=0.5, plot=TRUE, ntreeTry = 400,trace=TRUE,improve=0.05)
t_tunning

## fit randomforest with tune-mTry
rf_tunefit <- randomForest(as.factor(class)~.,importance=TRUE,data=heart_disease_train,mTry=4,proximity=TRUE)
rf_tunefit

## prediction for test data, post tunning
tune_predict <- predict(rf_tunefit, heart_disease_test)

## Confusion matrix, post fine tunning
CtMatrix <-table(predicted=tune_predict,observed=heart_disease_test$class)
CtMatrix

## Accuracy post tunning
Accuracy_tune <- sum(diag(ConMatrix))/sum(ConMatrix)*100
Accuracy_tune

#variable contributing most to the accuracy of the model.-----------------29
varImpPlot(rffit)
variable<- varImpPlot(rffit,sort = T,n.var=10,main="top 10 variables")

# To get the Quantitative value of the contributing variable----------------30
quantitative_value <- importance(rffit)
quantitative_value

# variables which is most been used in our Random forest.
used <- varUsed(rffit)-------------------31
used



#Affect of change in accuracy post,way data is sampled during training/testing.--------------32


#randomising the data--------------33

accuracy_sample <-heart_disease_Class1
rand_accuracy<- accuracy_sample[sample(nrow(accuracy_sample)),]

#splitting the data into test and training--------------------34
new_split<- floor((nrow(accuracy_sample)/5)*4)

#Training data -----------------35
new_train <- rand_accuracy[1:new_split,]

# Test Data ---------------------36
new_test <- rand_accuracy[(new_split+1):nrow(rand_accuracy), ]
count (new_test,class)

# change in accuracy post change in sampling using Random Forest --------------------37
rffit_sample <- randomForest(as.factor(class)~.,importance=TRUE,data=new_train)
rffit
summary(rffit_sample)
varImpPlot(rffit_sample)

# Prediction of Data post change in sampling---------------------38
forest_predict_sample <- predict(rffit_sample ,new_test)
forest_predict_sample

#Confusion Matrix for Random Forest post change in sampling ----------------39
ConMatrix_sample <-table(predicted=forest_predict_sample,observed=new_test$class)
ConMatrix_sample 
Accuracy_rdm_sample <- sum(diag(ConMatrix_sample))/sum(ConMatrix_sample)*100
Accuracy_rdm_sample



