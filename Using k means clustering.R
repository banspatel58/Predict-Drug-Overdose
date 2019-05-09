





##########################
##Part2
##########################
Drug_ID = read.csv("C:/Users/daxpa/Downloads/drug_ID_SMILES.csv", header = FALSE)
Drug_Features =  read.csv("C:/Users/daxpa/Downloads/drug_2D_features_dim_1024.csv", header = FALSE)

#set seed so that same cluster is generated every time code is compiled
set.seed(10)

# want to generate 7 clusters. Using nstart to select the best one from multiple initial configuration
Drug_Cluster = kmeans(Drug_Features, 7, nstart = 7)

Drug_ID$Result = as.factor(Drug_Cluster$cluster)

# List of drugs in Cluster 1
cluster1 = Drug_ID[which(Drug_ID$Result == 1), ]
print(cluster1[,3])

#List of drugs in Cluster 2
cluster2 = Drug_ID[which(Drug_ID$Result == 2), ]
print(cluster2[,3])

#List of drugs in Cluster 3
cluster3 = Drug_ID[which(Drug_ID$Result == 3), ]
print(cluster3[,3])

#List of drugs in Cluster 4
cluster4 = Drug_ID[which(Drug_ID$Result == 4), ]
print(cluster4[,3])

#List of drugs in Cluster 5
cluster5 = Drug_ID[which(Drug_ID$Result == 5), ]
print(cluster5[,3])

#List of drugs in Cluster 6
cluster6 = Drug_ID[which(Drug_ID$Result == 6), ]
print(cluster6[,3])

#List of drugs in Cluster 7
cluster7 = Drug_ID[which(Drug_ID$Result == 7), ]
print(cluster7[,3])

####################################################################################
######################################Report########################################
#As we can see in all of the clusters, that is from cluster 1 to 7 most of it has
#drugs that belong to same family and hence are clustered togather.
#Say for cluster 1, drugs listed in are THC (mostly) and hence it make sense that they 
#belong to same column.
#Similarly in the other drugs, from the name itself we could tell that they belong to
#same family. So stimulants tend to be in the same cluster.
####################################################################################
#####################################################################################
