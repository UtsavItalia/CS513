#Name   : UTSAV ITALIYA
#Subject: CS513 KNOWLEDGE DISCOVERY AND DATA MINING
#CWID   : 10475248

#for clearing the environment
rm(list=ls())

#creating the dataframe
dataframe <- data.frame(cols=c('a','b','c','d','e','f','g'),
                        X = c(1,5,4,4,1,4,2),
                        Y = c(1,3,4,3,2,4,1),
                        Z = c(1,4,5,4,1,4,2))

###########################################################################################################
                                #Question 3
###########################################################################################################

#using kmeans method
km <- kmeans(dataframe[,-1],2,nstart = 10)

#KMEANS CLUSTERING
km$cluster
#Answer: a,e,g are in one cluster that is (1).
#        b,c,d,f are in another cluster that is (2).

km$centers
#Answer: center of the two clusters are 1 = (1.33,1.33,1.33), 2 = (4.25,3.50,4.25)

###########################################################################################################
                                #Question 4
###########################################################################################################
#dist function is used to find the distance and hclust function to used with not including first column.
distance <- dist(dataframe[,-1])
hclust<-hclust(distance)

#cutree method is used to divide hclust into 2 groups using k=2
clust<- cutree(hclust,k = 2)

#for calculating center of two clusters
cluster_center = aggregate(dataframe[,-1],list(cluster=clust), mean)
cluster_center

