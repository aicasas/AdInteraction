'''
Alexis Casas
Advertising.csv

Exluding Date:
KNN Accuracy: 69.2%
Logistic Accuracy: 97.6%
SVM: 96.8

Logistic Provided the Best Accuracy. KNN method was not sufficient. 
SVM did provide a high accuracy as well. Although this is classified as really precisicon. 
We do not know what will happen in real life
'''

data<- read.csv('advertising.csv')
View(data)

#checking for missing variables:
which(is.na(data))

#install.packages('caret')
library(caret)

#Deleting Unimportant Columns:
#We decided to remove Ad Topic Line because of the large variety. 
#We believe that while some Topic Lines may be more intruiging than others, we are trying to determine
#what type of person will click on an intruiging add (assuming all are equally fascinating)
#Also deleting City because country is an approximately good way to categorize the location of the individual
#Group_Ass_Date file incorporates the Time variable and results in reduced accuracy.
#We also decide to delete Timestamp. 
#The time they are on the computer does not categorize the user.
#Tried it with country: Got 66% accuracy. Try Deleting
data <- data[,c(-5,-6,-7, -8, -9)]

#Encoding Categorical Data:
#We believe Country is involved in the user's portfolio so we keep this.

which(is.na(data)) #no missing data!

library(caTools) #activating package 
set.seed(42)

#Trying to predict whether they click on the add. 
split=sample.split(data$Clicked.on.Ad, SplitRatio= 0.75)
#splitting ratio can be anything. If it is higher than 95% then you are overfitting. 
#It is too familiar with the data that it can't understand the. The ratio where you get the most results should be picked
training_set=subset(data, split==TRUE)
test_set=subset(data, split==FALSE)

#Feature Scaling: Normalizing all units
training_set[,c()] = scale(training_set[,c()])
test_set[,c()] = scale(test_set[,c()])


#####Applying k-NN
#k-Nearest Neighbors #k can be any number
#install.packages('class')
library(class)
y_pred = knn(train = training_set[,-5],
             test = test_set[,-5],
             k=7,
             cl = training_set[,5],prob=TRUE)
#You train  on the training set, test on the test set, 5 nearest neighbors,
#cl is the response variable - the one you are predicting
#Confusion Matrix
con_matrix <- table(test_set[,5], y_pred)
accuracy=(95+79)/250
print(accuracy) #not ass good as we want but better once we deleted country 
#69.6%


#####Apply Logistic Regression
set.seed(42)
classifier = glm(formula = Clicked.on.Ad~.,family = binomial, data = training_set)
summary(classifier)
#***prob_predict is the Credit Scorecard with probabilities.
pro_pred = predict(classifier,type='response',test_set[,-5])   
y_pred_2 = ifelse(pro_pred>0.5,1,0)

#Confusion Matrix
con_matrix_2 = table(y_pred_2, test_set[,5])
accuracy_logistic= (123+118)/250
print(accuracy_logistic) #96.4%. This is much better than KNN

#Another way to write the glm function
#model <- glm(Clicked.on.Ad ~ Daily.Time.Spent.on.Site + Age + Area.Income + Daily.Internet.Usage + Male, data=data, family="binomial")
#summary(model)

# Store coefficients in another object
#coefs <- coef(model)
# Show the coefficients, just for fun



#####Applying SVM
library(e1071)

classifier= svm(formula = Clicked.on.Ad~., data=training_set, 
                type = 'C-classification', kernel= 'linear')
#kernal = linear is enforcing a linear classification. This is not always true nor most popular
#Kernal is an instance 
#C-classification is based on on maximizing the distance 
# it will predict a 1 or 0 rather than a number of 1s of 0s 

y_pred=predict(classifier, newdata = test_set[,-5])
con_matrix_3 <- table(test_set[,5], y_pred)

accuracy3=(123+118)/250
print(accuracy3) #96.4% Accuracy. Seems to be the best.

