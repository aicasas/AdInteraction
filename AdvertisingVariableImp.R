'''Advertising Variable Importance Added

For KNN Method:  
Daily Time Spent on SitEe: 100 Importance
Daily Internet Use: 45.475
Area Income: 2.842
Age:0


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
