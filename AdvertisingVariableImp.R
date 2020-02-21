'''Advertising Variable Importance Added

KNN Accuracy: 69.2%
Logistic Accuracy: 97.6%
SVM Accuracy: 96.8

Variable Importance:
P < 0.05
For Logistic Regression:
1. Daily Internet Usage: -6.473e-02 lowest P value, largest abs(z) value
2. Daily Time Spent: -1.892e-01
3. Area Income: -1.301e-04
4. Age: 1.755e-01 largest P value, still very significant

'''

data<- read.csv('advertising.csv')
View(data)

#checking for missing variables:
which(is.na(data))

#install.packages('caret')
library(caret)
data <- data[,c(-5,-6,-7, -8, -9)]

#Encoding Categorical Data:
which(is.na(data)) #no missing data!
library(caTools)

####KNN 
inTraining <- createDataPartition(data$Clicked.on.Ad, p = 0.75, list = FALSE)
training <- data[inTraining, ]
testing <- data[-inTraining, ]
modelFit <- train( Clicked.on.Ad~.,data=training, method="rpart" )  
#Variable Importance: How much the model uses the variables to make predictions
varImp(modelFit)

####SVM
library(rminer)
M <- fit(Clicked.on.Ad~., data=training, model="svm", kpar=list(sigma=0.10), C=2)
svm.imp <- Importance(M, data=training)
svm.imp

###Logistic Regression
classifier = glm(formula = Clicked.on.Ad~.,family = binomial, data = training)
summary(classifier)
