
#load required packages

library(cluster) 
library(factoextra) 
library(reshape2) 


#import data set

heart_disease_p <- read.csv(file.choose(),header = T)
attach(heart_disease_p)

#first 6 rows of the heart_disease_p 
head(heart_disease_p)


# choose columns for clustering
heart_disease_p1<-heart_disease_p[,2:12]
head(heart_disease_p1)

#overview of the data set
summary(heart_disease_p1)
str(heart_disease_p1)
dim(heart_disease_p1)
anyNA(heart_disease_p1)


#standardization data set

heart_disease_stand <- scale(heart_disease_p1)
head(heart_disease_stand)


#Assessing clustering tendency

tendency<-get_clust_tendency(heart_disease_stand, 20, graph = TRUE)
tendency$hopkins_stat


#choose the right number of expected clusters

fviz_nbclust(heart_disease_stand, kmeans, method = "wss") 
fviz_nbclust(heart_disease_stand, kmeans, method = "silhouette")


# K = 2
set.seed(123)

km.fit<-kmeans(heart_disease_stand,2,nstart = 50)

#plot cluster
fviz_cluster(km.fit,heart_disease_stand,ellipse.type = "convex",pointsize = 1,labelsize = 7)


#associate cluster column with heart_disease_p
cluster<-km.fit$cluster
heart_disease_p<-cbind(heart_disease_p,cluster)
head(heart_disease_p)





