#linear model
train = read.csv("C:/Users/daxpa/Downloads/train.csv")
test =  read.csv("C:/Users/daxpa/Downloads/test.csv")

######################################################################################
# Zip codes only used for looking up new data
# Exclude the zipcodes from the data set
######################################################################################
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
filtered_train = train[, !colnames(train) %in% drop_column]
filtered_test = test[, !colnames(test) %in% drop_column]

######################################################################################
# Cleaning Up data
# Replace the unknown and Homeless values from Zip codes by mode of zip codes from data
# Remove NA values from data set by getting mode
######################################################################################
library("gdata")

#define a mode function
getmode <- function(v) {
  
  uniqv <- unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

#replace the "Homeless" and "Unknown" values with NA for train data set
train$Home_Zip = unknownToNA(train$Home_Zip, unknown = "Homeless")
train$Home_Zip = unknownToNA(train$Home_Zip, unknown = "Unknown")

#replace the NA's from zip code with mode 
train$Home_Zip[which(is.na(train$Home_Zip))] = getmode(train$Home_Zip)

#replace the NA's from Education, Employment & Military with their respective mode 
train$Military[which(is.na(train$Military))] = getmode(train$Military)
train$Employment[which(is.na(train$Employment))] = getmode(train$Employment)
train$Education[which(is.na(train$Education))] = getmode(train$Education)

######################################################################################

# Using glm model on the train data seet and filtered_data

######################################################################################

#use the glm function
model = glm(Fentanyl ~ . -1, data = train, family = "binomial")

#predict the results
test$Fentanyl = predict(model, test, type = "response")
submission = test[,c("id", "Fentanyl")]
submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)

write.csv(submission, "C:/Users/daxpa/Downloads/Initial Submit.csv")

#use the glm function on filtered data set
new_model = glm(Fentanyl ~ ., data = filtered_train, family = "binomial")
summary(model)

#predict the results
test$Fentanyl = predict(model, test, type = "response")
submission = filtered_train[,c("id", "Fentanyl")]

submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)


######################################################################################

# Using family as poisson for glm model using: train data set and filtered data set

######################################################################################
#use the glm function
model = glm(Fentanyl ~ . -1, data = filtered_train, family = "poisson" )

#predict the results
test$Fentanyl = predict(model, filtered_test, type = "response")
submission = test[,c("id", "Fentanyl")]

submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)
write.csv(submission, "C:/Users/daxpa/Downloads/poisson.csv")

#use the glm function
model = glm(Fentanyl ~ . , data = filtered_train, family = poisson(link = "log") )

#predict the results
test$Fentanyl = predict(model, filtered_test, type = "response")
submission = test[,c("id", "Fentanyl")]

submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)
write.csv(submission, "C:/Users/daxpa/Downloads/UsingFilteredData_poisson.csv")


######################################################################################
# Using gbm model: Using train data and filtered_train data
######################################################################################
# Using train data
model_gbm=gbm(Fentanyl ~ ., data = train, distribution="bernoulli", 
              interaction.depth = 3, n.trees=100)
test$Fentanyl = predict(model_gbm, test, n.trees=100, type="response")

submission = test[,c("id", "Fentanyl")]
submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)

write.csv(submission, "C:/Users/daxpa/Downloads/gbm.csv")

# Using Filtered Data
model_gbm=gbm(Fentanyl ~ .-1, data=filtered_train, distribution="bernoulli", interaction.depth = 3,
              n.trees=100)
test$Fentanyl = predict(model_gbm, test, n.trees=100, type="response")

submission = test[,c("id", "Fentanyl")]
submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)

write.csv(submission, "C:/Users/daxpa/Downloads/UsingFilterData_gbm.csv")

######################################################################################
# Using adaboost
######################################################################################
library(adabag)
#train = train[,1:157]

Using_Adaboost = boosting( Fentanyl ~ ., data = train,
                           boos=TRUE, mfinal=20, control = rpart.control(cp = -1) )

model_gbm=gbm(Fentanyl ~ ., data=train,distribution="bernoulli", interaction.depth = 3,
              n.trees=100)
test$Fentanyl = predict(model_gbm, test, n.trees=100, type="response")

