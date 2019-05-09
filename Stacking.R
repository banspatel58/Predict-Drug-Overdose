train = read.csv("C:/Users/daxpa/Downloads/train.csv")
test =  read.csv("C:/Users/daxpa/Downloads/test.csv")

#define a mode function
getmode <- function(v) {
  
  uniqv <- unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

#replacing the NA's from Education, Employment & Military with their respective mode 
train$Military[which(is.na(train$Military))] = getmode(train$Military)
train$Employment[which(is.na(train$Employment))] = getmode(train$Employment)
train$Education[which(is.na(train$Education))] = getmode(train$Education)
# stacking
library(gbm)
library(pROC)

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
                 "Benztropine", "Norpseudoephedrine", "Home_Zip" )
train = train[, !colnames(train) %in% drop_column]
test = test[, !colnames(test) %in% drop_column]


data = sample(nrow(train),0.7 * nrow(train),F)

train = train[ data, ]
test = train[-data, ]

# Factoring the fentanyl object before measuring it in random forrest
train$Fentanyl = as.character(train$Fentanyl)
train$Fentanyl = as.factor(train$Fentanyl)


model_glm=glm(Fentanyl ~ . -1, data=train, family="binomial")

model_gbm=gbm(Fentanyl ~ . -1, data = train, distribution = "bernoulli",
              interaction.depth = 3, n.trees=100)

y_glm = predict(model_glm, test, type="response")
auc(test$Fentanyl, y_glm)

y_gbm = predict(model_gbm, test, n.trees=100, type="response")
auc(test$Fentanyl, y_gbm)

test$y_glm=y_glm
test$y_gbm=y_gbm

model_stack = glm(Fentanyl ~ y_gbm + y_glm +.:y_gbm + .:y_glm
                  , data = test, family = "binomial")


test$y_stack = predict(model_stack, data = test, type = "response")
auc(test$Fentanyl,test$y_stack)

write.csv(submission, "C:/Users/daxpa/Downloads/Fourth3 Attempt.csv")



Drug_ID = read.csv("C:/Users/daxpa/Downloads/drug_ID_SMILES.csv", header = FALSE)
Drug_Features =  read.csv("C:/Users/daxpa/Downloads/drug_2D_features_dim_1024.csv", header = FALSE)

#set seed so that same cluster is generated every time code is compiled
set.seed(10)

# want to generate 7 clusters. Using nstart to select the best one from multiple initial configuration
Drug_Cluster = kmeans(Drug_Features, 7, nstart = 7)

Drug_ID$Result = as.factor(Drug_Cluster$cluster)

# List of drugs in Cluster 1
cluster1 = Drug_ID[which(Drug_ID$Result == 1), ]

#List of drugs in Cluster 2
cluster2 = Drug_ID[which(Drug_ID$Result == 2), ]

#List of drugs in Cluster 3
cluster3 = Drug_ID[which(Drug_ID$Result == 3), ]

#List of drugs in Cluster 4
cluster4 = Drug_ID[which(Drug_ID$Result == 4), ]

#List of drugs in Cluster 5
cluster5 = Drug_ID[which(Drug_ID$Result == 5), ]

#List of drugs in Cluster 6
cluster6 = Drug_ID[which(Drug_ID$Result == 6), ]

#List of drugs in Cluster 7
cluster7 = Drug_ID[which(Drug_ID$Result == 7), ]

cluster1 = cluster1[,3]

out = cbind(Drug_Features, clusterNum = Drug_Cluster$cluster)
