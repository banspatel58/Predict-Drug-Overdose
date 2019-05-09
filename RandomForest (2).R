######################################################################################

# Using Random Forest

######################################################################################
library(randomForest)
library(caret)
library(e1071)

#linear model
train = read.csv("C:/Users/daxpa/Downloads/train.csv")
test =  read.csv("C:/Users/daxpa/Downloads/test.csv")

#replacing the NA's from Education, Employment & Military with their respective mode 
train$Military[which(is.na(train$Military))] = getmode(train$Military)
train$Employment[which(is.na(train$Employment))] = getmode(train$Employment)
train$Education[which(is.na(train$Education))] = getmode(train$Education)

# Zip codes only used for looking up new data. Exclude the zipcodes from the data set
train = train[, 1:157]
test = test[,1:156]

########################################################################################
# Filter data set by removing columns 
# Remove Columns with all zero's in it
########################################################################################
drop_column = c( "Acetone", "Isopropanol", "Phentermine", "Naproxen", "Azithromycin",
                 "Piperacillin", "Lamotrigine", "Norvenlafaxine", "Bupropion", 
                 "Guaifenesin", "Olanzapine", "Butalbital", "Demoxepam", "Midazolam", 
                 "Flubramazolam", "Hydrochlorothiazide", "Lidocaine", "Dicyclomine", 
                 "o.Desmethyltramadol", "Psychoactivesubstances", "Difluoroethane", 
                 "Amiodarone", "Methocarbamol", "Tizanadine", "Neurologicals", "Zolpidem",
                 "Benztropine", "Norpseudoephedrine" )
train = train[, !colnames(train) %in% drop_column]
test = test[, !colnames(test) %in% drop_column]



data = sample(nrow(train), 0.7*nrow(train), F)

train = train[data, ]
test = train[-data, ]

# Factoring the fentanyl object before measuring it in random forrest
train$Fentanyl = as.character(train$Fentanyl)
train$Fentanyl = as.factor(train$Fentanyl)

# A Random Forest model with default parameters
model_rf1 = randomForest( Fentanyl ~ . - 1, data = train, importance = T )

# printing the model to get the confusion matrix and out-of-bag error estimate
print(model_rf1)
# looking over the attributes of the random forest model
attributes(model_rf1)
# Extracting confusion matrix
print(model_rf1$confusion)

# Predicting the model based on the train data set
predict_rf = predict(model_rf1, train, type = "class")

# predictions for Fentanyl
print(head(predict_rf))
print(head(train$Fentanyl))

# getting the confusion matrix
print(confusionMatrix(predict_rf, train$Fentanyl))

# predicting the model based on test data set
predict2_rf = predict(model_rf1, test, type = "class")

# confusion matrix or 
table(predict2_rf, test$Fentanyl)

confusionMatrix(predict2_rf, test$Fentanyl)

# plot the random forest model
plot(model_rf1)



# using for loop to find out the right mtry value
a = c()
i = 9
for( i in 5:10){
  
  model_randomForest = randomForest( Fentanyl ~ . -1, data = train, ntree = 500,
                                     mtry = i, importance = T)
  prediction = predict(model_randomForest, data = test, type = "class")
  
  a[i-10] = mean(prediction == test$Fentanyl)
  
}
a

plot(12:16, a)
