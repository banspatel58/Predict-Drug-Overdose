#linear model
train = read.csv("C:/Users/daxpa/Downloads/train.csv")
test =  read.csv("C:/Users/daxpa/Downloads/test.csv")

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

#convert zip codes in to list
#Home_Zip = as.character(train$Home_Zip)

#use the glm function
model = glm(Fentanyl ~ ., data = train, family = "binomial")
model$xlevels[["Home_Zip"]] = union(model$xlevels[["Home_Zip"]], levels(test$Home_Zip))
summary(model)

#predict the results
test$Fentanyl = predict.glm(model, test, type = "response")
submission = test[,c("id", "Fentanyl")]
print(head(submission))

submission$Fentanyl[is.na(submission$Fentanyl)] = mean(submission$Fentanyl, na.rm = TRUE)

write.csv(submission, "C:/Users/daxpa/Downloads/Initial Submit.csv")

library(gbm)
train=train[sample(nrow(train),nrow(train),F),]

train1=train[1:500,]
train2=train[501:1000,]

model_gbm = gbm(Fentanyl ~ ., data=train1, distribution="bernoulli", interaction.depth = 3,
              n.trees=100)
y_gbm =predict(model_gbm, train2, n.trees=100, type="response")

auc( train2$Fentanyl, y_gbm)

