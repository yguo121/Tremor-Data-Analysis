########################################
############# Clustering ###############
########################################

library(cluster)
library(fpc)

load("../data/train_set/train_5.RData")
dt_train <- as.data.frame(dt_train)

# PCA first 2 compoents clustering
clusters <- kmeans(dt_train[,1:2],2)
plotcluster(dt_train[,1:18], clusters$cluster)
clusplot(dt_train[,1:18], clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


# silhouette plot
dissE <- daisy(dt_train[,1:18]) 
dE2   <- dissE^2
sk2   <- silhouette(clusters$cl, dE2)
plot(sk2)

