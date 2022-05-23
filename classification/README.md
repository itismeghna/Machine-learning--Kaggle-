# Machine-learning--Kaggle-Code: 


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

# removing X1 identifier in order to blind the subject---------------------4
heart_disease_Class1<- heart_disease_Class1[ , !(names(heart_disease_Class1) %in% c("X1","Patient_ID"))]

##identifying the datatype of each variable, post hardcoding-----------5
str(heart_disease_Class1)

#summary function to identifying the missing values.-------------------6
summary(heart_disease_Class1)

# counting the number of heart patient and non heart patient.-----------7
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

#confusion matrix to determine true positive and true negative based on the selected model.-------------14

ConMatrix <-table(predicted=nb_predict,observed=heart_disease_test$class)
ConMatrix 

#Calculating the Accuracy percentage  from confusion matrix--------15

Accuracy <- sum(diag(ConMatrix))/sum(ConMatrix)*100
Accuracy

#Calculating the percentage of missclassification.-----------------------16

Missclassification <- 100-sum(diag(ConMatrix))/sum(ConMatrix)*100
Missclassification

#Calculating the probabilities of the outcome.--------------------------18
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

# Determining area under the curve for naive Bayes --------------------21
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


Q2. Now choose one classifier to further optimize.------------27 

# determining the OOB for the data., it is been observed that error rate cannot be reduced 
#post trees =500
plot(rffit)

# tunning our random forest model----------------28
## tune the model
t_tunning <- tuneRF(heart_disease_train[,-20],as.factor(heart_disease_train$class), stepFactor=0.5, plot=TRUE, ntreeTry = 500,trace=TRUE,improve=0.05)
t_tunning

## fit randomforest with tune-mTry
rf_tunefit <- randomForest(as.factor(class)~.,importance=TRUE,data=heart_disease_train,mTry=8,proximity=TRUE)
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

# To get the Quantitative value of the contributing variable--------------30
quantitative_value <- importance(rffit)
quantitative_value

# variables which is most been used in our Random forest.----31
used <- varUsed(rffit)
used



#Affect of change in accuracy post,way data is sampled during training/testing.--32


#randomizing the data--------------33

accuracy_sample <-heart_disease_Class1
rand_accuracy<- accuracy_sample[sample(nrow(accuracy_sample)),]

#splitting the data into test and training--------------------34
new_split<- floor((nrow(accuracy_sample)/5)*4)

#Training data -----------------35
new_train <- rand_accuracy[1:new_split,]

# Test Data ---------------------36
new_test <- rand_accuracy[(new_split+1):nrow(rand_accuracy), ]
count (new_test,class)

# change in accuracy post change in sampling using Random Forest --37
rffit_sample <- randomForest(as.factor(class)~.,importance=TRUE,data=new_train)
rffit
summary(rffit_sample)
varImpPlot(rffit_sample)

# Prediction of Data post change in sampling---------------------38
forest_predict_sample <- predict(rffit_sample ,new_test)
forest_predict_sample

#Confusion Matrix for Random Forest post change in sampling -------39
ConMatrix_sample <-table(predicted=forest_predict_sample,observed=new_test$class)
ConMatrix_sample 
Accuracy_rdm_sample <- sum(diag(ConMatrix_sample))/sum(ConMatrix_sample)*100
Accuracy_rdm_sample






Q1. For each classifier, please answer the following:

Using Classifier as naive bayes

1.Did you undertake any prepossessing? If so, why?  Refer code (3-6)

Ans1) Yes, I did undertake processing of data in order to make my input data file organize in order to run the algorithm fluently following are the steps adapted.
 
a)Data need to be in numeric data type for machine learning algorithms to use the data to make predictions, so for our given dataset variable  “pace-maker”, “drug”, “fam_hist” is a character data type , so we converted it into numeric data type which is called as dummy encoded.

b) Ideally we need scaling feature to our data when we use the classifier such as KNN (K-Nearest Neighbors),Logistic Regression which is calculated on the bases of distance, but for our data we are using Naive Bayes and Random forest which is not based on distance and can var range of feature , so scaling is not required.

c) Removing missing data, for our dataset we don't have any missing value , but If we have missing data point it should be removed , if not removed accuracy of the model is heavily affected.

d) Removing the column “X1" "Patient_ID”, in order to blind the subjects
from the given dataset.

2.Run the classifier with default parameters. Print the output from Weka.

a) How accurately can the classifier predict those that develop heart disease? What is in the output that signifies this?------------ ( code:14,15,7)

Ans a) When counting the number of subject based on class column which is been labeled as No Heart disease as = 0  and Heart disease= 1, below are the count for test data set, we can see that count for 0 = 102 and 1 = 128





Post running Confusion Matix it is been observed that, the number of subjects expected to be having heart disease post running the algorithm results to be 103 that is true positive, ideally it has to be 128 as per ground truth,. So accuracy of classifier in predicting those who develop heart disease is (103/128)*100 =80.5 %








b) How many people are misclassified as developing heart disease? Where is this answer found in the output? code:(14,15,16 ,7)
         
Ans b) Post running confusion matrix, we could be analyses that 28 count has been misclassified it is False positive count , which is an erroneous count by the model its is falsely considered as positive for heart disease.
Q.3 Plot and submit the ROC curve for the class that develops heart disease. What is another measure of accuracy commonly used? Please provide this. ( code 20,21 and 14 for confusion matrix)

Ans3) Plotted the ROC curve for the naive bayes. And the area under curve.




Another method to measure accuracy is confusion matrix: code 14 and 15.




b) Q1. For each classifier, please answer the following:

Using Classifier as Random forest

1.Did you undertake any prepossessing? If so, why? (22,3-6)

Ans1) a)Yes, I did undertake processing of data in order to make my input data file organize in order to run the algorithm fluently following are the steps adapted.

b) Converting labeled variable into factor for an algorithm to run for random forest

b)Data need to be in numeric data type for machine learning algorithms to use the data to make predictions, so for our given dataset variable  “pace-maker”, “drug”, “fam_hist” is a character data type , so we converted it into numeric data type which is called as dummy encoded.

b) Ideally we need scaling feature to our data when we use the classifier such as KNN (K-Nearest Neighbors),Logistic Regression which is calculated on the bases of distance, but for our data we are using Naive Bayes and Random forest which is not based on distance and can var range of feature , so scaling is not required.

c) Removing missing data, for our dataset we don't have any missing value , but If we have missing data point it should be removed , if not removed accuracy of the model is heavily affected.

d) Removing the column “X1" "Patient_ID”, in order to blind the subjects
from the given dataset.



a) How accurately can the classifier predict those that develop heart disease? What is in the output that signifies this?------------ ( code: 24,7)

Ans a) When counting the number of subject based on class column which is been labeled as No Heart disease as = 0  and Heart disease= 1, below are the count for test data set, we can see that count for 0 = 73 and 1 = 112





Post running Confusion Matix it is been observed that, the number of subjects expected to be having heart disease post running the algorithm results to be 103 that is true positive, ideally it has to be 128 as per ground truth,. So accuracy of classifier in predicting those who develop heart disease is (112/128)*100 =87.5 %


b) How many people are misclassified as developing heart disease? Where is this answer found in the output? code:(24,7)
         
Ans b) Post running confusion matrix, we could be analyses that 29 count has been misclassified it is False positive count , which is an erroneous count by the model its is falsely considered as positive for heart disease.



Q.3 Plot and submit the ROC curve for the class that develops heart disease. What is another measure of accuracy commonly used? Please provide this. ( code 20,21 and 14 for confusion matrix)

Ans3) Plotted the ROC curve for the Random Forest. And the area under curve. Code (24,25,26)




Another method to measure accuracy is confusion matrix, which we have seen above.


Q2. Now choose one classifier to further optimize. 

1.Why did you choose this classifier over the other? (24,15)

Ans 1)The reason why , I selected Random forest over Naive Bayes
Firstly the accuracy attend by Random is high , so further optimization will make the model more efficient.

2.Random forest has various parameters to be played upon such as tree size,node to make the model more accurate.

3.Moreover it works well with numerical and categorical data. 

4.The OOB estimate of error rate is a useful measure to discriminate between different random forest classifiers, which helps to make the model reliable


2.Briefly explain how this classifier works from a theoretical point of view.

Following is the way the random forest classifiers work.
1.Random Forests is extensively used across many fields. 
2.Random Forests generate a large number of decision trees, each constructed using a different subset of our training set. 
3.Random forest, like its name implies, consists of a large number of individual decision trees that operate as an ensemble. Each individual tree in the random forest spits out a class prediction and the class with the most votes becomes our model’s prediction

4.These subsets are usually selected by sampling at random and with replacement from the original data set. 
5.The decision trees are then used to identify a classification consensus by selecting the most common output (mode). 








Reference: https://www.youtube.com/watch?v=ypO1DPEKYFo



3.Try to optimize the classifier to achieve a higher accuracy (no matter how small) than first found. Remember that we have a particular focus on predicting those that develop heart disease.


a)Were there any features that could be removed? How you could you determine this using Weka? Please print the output that helped you make this decision. ( code: 29,30,31)

Ans a) yes, there were variables in the data set which can be removed as they did’nt participated in the building up the accuracy of the random forest model, which can be determined by using “varImpPlot” as this function would plot a graph showing the level of its participation in accuracy.



This figure explains the top 10 variables which affect the most to the accuracy of the random forest and also if you could see , variable such as drugs, smaoker fam-hist has no contribution , so this can be removed and also if you observe in the below image., certain variables has zero or even less contribution in our random forest model.(meanDecrease Accuracy)








b) Did changing the way data is sampled during training/testing affect the accuracy? (code: 39,24)

Ansb) Yes, Changing the way data is been sampled during training and testing did have affect on the accuracy, the percentage. Initially the accuracy obtained for the model was 

When sampling was 75:25,  the accuracy was 80.43 and when sampling changed to 80:20 the accuracy was 83.69%


                
Thus we should use k-cross fold validation k=3, we should repeat train/test method 3 times were each time data should be split randomly, we should this because splitting  the data just once may not represent well rounded off training or test.


C.What about some of the internal parameters specific to the classifier? Please explain how one of these parameters can affect accuracy. ( REFER CODE:28 and all the code below it just above 29)

In order to increase the accuracy for the random forest model we determined the OBB error which showed minimum error for mtry as 4, considering mtry as 4 for our model the accuracy has marginally improved  .Also changing number of trees for our model does not bring in any change in accuracy as its  been observed for the number of tree at 500 have minimum error.but the model have definitely showed an improvement in terms of sensitivity,,
We can observe the value of true positive have increased.




4.In general, a classifier is only as good as the data it is trained on. Please comment on what is needed from training data to train a good classifier. How can utilizing classifiers help feed back into healthcare settings with regards to data collection?  

Ans 4) Following are the properties a good training data should have , as our model is been trained on the bases of the data which we input, so in order to train our model perfectly we need to feed our classifiers with quality data, below are the qualities:
1.Less of noise , our training data should have less of noise i.e avoid data values which have null affect on the accuracy of the model or hamper the effectiveness of the model.
2.A good training data should have uniformity in the data format, such as  maintaining the uniformity of data types.
3.Your training data should not be biased, i.e it should cover all the possible scenario which your model can expect from test sample.
4.Missing value should be avoid, as it greatly hamper the accuracy of the model.
5.As most of the model are trained for numeric values, we should try and maintains the data type in numeric format.
6.Labeling of data should be practiced, as it helps the model greatly. 

As data collection is the most crucial step for any data analysis, because our output heavily depend on the collection of data, it is been observed that most of the conclusion tend to be erroneous because of the faulty data collection methods.From classifier results we can analyses the influence of different parameter on the classification output for various disease types.Few parameters are found, which have no influence on chances of diseases such parameter can be identified and can be excluded at the initial stage of data collection as they account for the noise.Furthermore data with missing value should be avoided as these missing values could change the resultant outcome drastically, for an instance a patient with dietetics should not have with missing sugar level data point as these data value are critical in decision making.Ineffective data collection , causes lot of trouble in the data linkage , which is been used by statistician and epidemiologist for various statistical studies, which can be carried out seamlessly if we have good data integrity.The cleaner the input , the cleaner the output. 
 


