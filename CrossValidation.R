#linear model
train = read.csv("C:/Users/daxpa/Downloads/train.csv")
test =  read.csv("C:/Users/daxpa/Downloads/test.csv")

train = train[,  1:157 ]
test = test[, 1:156]

library(pROC)
library(gbm)

#define a mode function
getmode <- function(v) {
  
  uniqv <- unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}
#replacing the NA's from Education, Employment & Military with their respective mode 
train$Military[which(is.na(train$Military))] = getmode(train$Military)
train$Employment[which(is.na(train$Employment))] = getmode(train$Employment)
train$Education[which(is.na(train$Education))] = getmode(train$Education)

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


# Splitting the data to do 5 fold cross validation family = binomial, gaussian 

#####################################################################################

# Data set for Fold1

###################################################################################
# Shuffling data in train data set randomly to do cross validation
train = train[sample(nrow(train), nrow(train), F),]

train1 = train[1:800, ]
test1 = train[801:1000, ]

# Model_glm: Family: Binomial
model1 = glm( Fentanyl ~ . -1 , data = train1, family = "binomial")
pred1 = predict(model1, test1, type = "response")

# Model_glm: Family: Gaussian
model6 = glm( Fentanyl ~ . -1, data = train1, family = "gaussian")
pred6 = predict(model6, test1, type = "response")

# Model_glm: Family: Poisson
model11 = glm( Fentanyl ~ . -1, data = train1, family = poisson(link = "log") )
pred11 = predict( model11, test1, type = "response" )

# Model_gbm
model_gbm1 = gbm(Fentanyl ~ . -1, data = train1, distribution="bernoulli", 
              interaction.depth = 3, n.trees=100)
pred_gbm1 = predict(model_gbm1, test1, n.trees=100, type="response")

roc1 = roc(test1$Fentanyl, pred1)
auc1 = auc(roc1)

roc6 = roc(test1$Fentanyl, pred6)
auc6 = auc(roc6)

roc11 = roc(test1$Fentanyl, pred11)
auc11 = auc(roc11)

roc_gbm1 = roc( test1$Fentanyl, pred_gbm1 )
auc_gbm1 = auc(roc_gbm1)

###################################################################################

# Data set for Fold2

###################################################################################

#train = train[sample(nrow(train), nrow(train), F),]
train2 = train[ 200:1000, ]
test2 = train[ 1:199, ]

# Model_glm: Family: Binomial
model2 = glm( Fentanyl ~ . -1, data = train2, family = "binomial")
pred2 = predict(model2, test2, type = "response")

# Model_glm: Family: Gaussian
model7 = glm( Fentanyl ~ . -1, data = train2, family = "gaussian")
pred7 = predict(model7, test2, type = "response")

# Model_glm: Family: Poisson
model12 = glm( Fentanyl ~ . -1, data = train2, family = poisson(link = "log") )
pred12 = predict(model12, test2, type = "response")

# Model_gbm
model_gbm2 = gbm(Fentanyl ~ . -1, data = train2, distribution="bernoulli", 
                 interaction.depth = 3, n.trees=100)
pred_gbm2 = predict(model_gbm2, test2, n.trees=100, type="response")

roc2 = roc(test2$Fentanyl, pred2)
auc2 = auc(roc2)

roc7 = roc(test2$Fentanyl, pred7)
auc7 = auc(roc7)


roc12 = roc(test2$Fentanyl, pred12)
auc12 = auc(roc12)

roc_gbm2 = roc( test2$Fentanyl, pred_gbm2 )
auc_gbm2 = auc(roc_gbm2)

###################################################################################

# Data set for Fold3

###################################################################################

#train = train[sample(nrow(train), nrow(train), F),]
train3 = train[c( 1:100, 300:1000), ]
test3 = train[101:299, ]

# Model_glm: Family: Binomial
model3 = glm( Fentanyl ~ . -1, data = train3, family = "binomial")
pred3 = predict(model3, test3, type = "response")

# Model_glm: Family: Gaussian
model8 = glm( Fentanyl ~ . -1, data = train3, family = "gaussian")
pred8 = predict(model8, test3, type = "response")

# Model_glm: Family: Poisson
model13 = glm( Fentanyl ~ . -1, data = train3, family = poisson(link = "log"))
pred13 = predict(model13, test3, type = "response")

# Model_gbm
model_gbm3 = gbm(Fentanyl ~ . -1, data = train3, distribution="bernoulli", 
                 interaction.depth = 3, n.trees=100)
pred_gbm3 = predict(model_gbm3, test3, n.trees=100, type="response")


roc3 = roc(test3$Fentanyl, pred8)
auc3 = auc(roc3)

roc8 = roc(test3$Fentanyl, pred8)
auc8 = auc(roc8)

roc13 = roc(test3$Fentanyl, pred13)
auc13 = auc(roc13)

roc_gbm3 = roc( test3$Fentanyl, pred_gbm3 )
auc_gbm3 = auc(roc_gbm3)

###################################################################################

# Data set for Fold4

###################################################################################

#train = train[sample(nrow(train), nrow(train), F),]
train4 = train[c( 1:200, 400:1000 ), ]
test4 = train[201:399, ]

# Model_glm: Family: Binomial
model4 = glm( Fentanyl ~ . -1, data = train4, family = "binomial")
pred4 = predict(model4, test4, type = "response")

# Model_glm: Family: Gaussian
model9 = glm( Fentanyl ~ . -1, data = train4, family = "gaussian")
pred9 = predict(model9, test4, type = "response")

# Model_glm: Family: Poisson
model14 = glm( Fentanyl ~ . -1, data = train4, family = poisson(link = "log"))
pred14 = predict(model14, test4, type = "response")

# Model_gbm
model_gbm4 = gbm(Fentanyl ~ . -1, data = train4, distribution="bernoulli", 
                 interaction.depth = 3, n.trees=100)
pred_gbm4 = predict(model_gbm4, test4, n.trees=100, type="response")

roc4 = roc(test4$Fentanyl, pred4)
auc4 = auc(roc4)

roc9 = roc(test4$Fentanyl, pred9)
auc9 = auc(roc9)

roc14 = roc(test4$Fentanyl, pred14)
auc14 = auc(roc14)

roc_gbm4 = roc( test4$Fentanyl, pred_gbm4 )
auc_gbm4 = auc(roc_gbm4)

###################################################################################

# Data set for Fold5

###################################################################################

train = train[sample(nrow(train), nrow(train), F),]
train5 = train[c(500:1000, 1:300), ]
test5 = train[301:499, ]

# Model_glm: Family: Binomial
model5 = glm( Fentanyl ~ . -1, data = train5, family = "binomial")
pred5 = predict(model5, test5, type = "response")

# Model_glm: Family: Gaussian
model10 = glm( Fentanyl ~ . -1, data = train5, family = "gaussian")
pred10 = predict(model10, test5, type = "response")

# Model_glm: Family: Poisson
model15 = glm( Fentanyl ~ . -1, data = train5, family = poisson(link = "log"))
pred15 = predict(model15, test5, type = "response")

# Model_gbm
model_gbm5 = gbm(Fentanyl ~ . -1, data = train5, distribution="bernoulli", 
                 interaction.depth = 3, n.trees=100)
pred_gbm5 = predict(model_gbm5, test5, n.trees=100, type="response")

roc5 = roc(test5$Fentanyl, pred5)
auc5 = auc(roc5)

roc10 = roc(test5$Fentanyl, pred10)
auc10 = auc(roc10)

roc15 = roc(test5$Fentanyl, pred15)
auc15 = auc(roc15)

roc_gbm5 = roc( test5$Fentanyl, pred_gbm5 )
auc_gbm5 = auc(roc_gbm5)

#####################################################################################

# Calculating means of aucs's to determine the best model

#####################################################################################
Mean_Using_Binomial = mean( auc1, auc2, auc3, auc4, auc5)

Mean_Using_Gaussian = mean( auc6, auc7, auc8, auc9, auc10 )

Mean_Using_Poisson = mean( auc11, auc12, auc13, auc14, auc15 )

Mean_Using_GBM = mean( auc_gbm1, auc_gbm2, auc_gbm3, auc_gbm4, auc_gbm5 )

# Average of AUC's using glm model: Family Binomial
Mean_Using_Binomial

# Average of AUC's using glm model: Family Gaussian
Mean_Using_Gaussian

# Average of AUC's using glm model: Family Poisson
Mean_Using_Poisson

# Average of AUC's using gbm model
Mean_Using_GBM

#####################################################################################

# Since gbm model gives higher average AUC's then all other model's 
# I chose gbm model to predict for Fentanyl

#####################################################################################



#use random forest
library(caret)

data_control = trainControl(method = "cv", number = 5, verboseIter = F)

model6 = train(Fentanyl ~ ., data = train, method = "rf", trControl = data_control)

#using rpart
library(rpart)
library(rpart.plot)
rpart_model <- rpart(formula = Fentanyl ~ ., data = train ,method = "class")

# Display the results
rpart.plot(x = rpart_model, yesno = 2, type = 0, extra = 0)

library(caret)
#creating a confusion matrix
# Generate predicted classes using the model object
class_prediction <- predict(object = rpart_model, newdata = test, type = "class")  

# Calculate the confusion matrix for the test set
confusionMatrix(data = class_prediction,       
                reference = test$Race)  

