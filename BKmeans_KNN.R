######
# Implementation of Bisecting K-Means and KNN Classifier Functions
#####

require(RColorBrewer)
require(class)
require(caret)

# Plot clustering result
plot_clustering_result <- function(data.df, result, title, k){
  # Plot using two columns in the data
  plot(data.df[, c(2,4)], col = brewer.pal(k, "Set1")[result[[1]]], pch = '.',
       cex = 3, main = title)
}

################################################
# Implement bisecting k means.
# Input:
# data.df: data frame loaded using load_data() function
# iter.max: Max. number of trials for kmeans
# k: Number of clusters to find in bisecting k-means

# Output:
# The output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split,the one with maximum SSE is chosen.
# When performing kmeans, two random points the cluster with largest SSE as centers at every iteration is picked. 
# Ensure that the two centers being randomly picked are not the same.
###############################################

bkm <- function(data.df, iter.max, k){
sse <- c(1)
originaldata.df <- load_data_clustering()
data.df <- originaldata.df[c(2:5)]
data.list <- list()
data.list[[1]] = data.frame(data.df)
#df <- data.frame(matrix(unlist(l), nrow=132, byrow=T))   list to df
copydata.list <- split(data.df, seq(nrow(data.df)))  #df to list


while(length(data.list)<k)
{
  lengthdatalist <- length(data.list)
  lengthsse<- length(sse)
  if(lengthdatalist == 1){
    index = 1} else {
      maxsse=-999999999
      for(i in 1:lengthsse){
        
        if (sse[i] > maxsse){
          index = i
          maxsse=sse[i]}
      }
    }
  tosplit<- as.data.frame(data.list[[index]])

  library(rlist)
  #data.list= list.remove(data.list, index)
  #data.list = da[lapply(suu,length)!=0]
  #  data.list = data.list[-which(lapply(data.list,is.null) == T)]  
  minsse = 9999999999
  for(i in 1:iter.max)
  {#as.matrix(as.data.frame(lapply(data.list[[index]], as.numeric)))
    kmeans1 <- kmeans(tosplit,2)
    if (kmeans1$tot.withinss < minsse){
      minsse = kmeans1$tot.withinss
      best = kmeans1}
  }
  
  clust1=c()
  clust2=c()
  
  partition = as.data.frame(best$cluster)
  colnames(partition)= "div"
  rname= row.names(partition)
  for(i in 1: nrow(partition))
    if(partition$div[i]==1){ clust1<- c(clust1,as.numeric(rname[i]))} else
    { clust2<- c(clust2,as.numeric(rname[i]))}      
  
  
  #clust1 <- which(as.data.frame(best$cluster) == 1)
  #clust_1 <- names(clust1)
  
  lenclust1 <- length(clust1)
  df1=data.frame(Doubles=double(),Doubles=double(),Doubles=double(),Doubles=double())
  df2=data.frame(Doubles=double(),Doubles=double(),Doubles=double(),Doubles=double())
  for (j in 1:lenclust1)
  {  
    df1[nrow(df1) + 1,]  =  data.df[clust1[j],]
  } 
  
  #clustlist1=data.frame(matrix(unlist(clustlist1), nrow=lenclust1, byrow=T))
  #clust2 <- which (as.data.frame(best$cluster) == 2)
  #clust_2 <- names(clust2)
  
  lenclust2 <- length(clust2)
  for (j in 1:lenclust2)
  {
    df2[nrow(df2) + 1,] = data.df[clust2[j],]
  }
  
  
  if (length(data.list)==1)
  {
    data.list[[1]] <-df1
    data.list[[2]] <- df2
    #data.list = data.list[-which(lapply(data.list,is.null) == T)]
  }else{
    lengthdatalist1 <- length(data.list)
    #list.append(data.list,clustlist1, clustlist2)}
    
    
    data.list[[length(data.list)+1]] <-  df1
    data.list[[length(data.list)+1]] <-  df2
    
    
    for (i in (index+1):(length(data.list))-1)
    {
      data.list[[i]]= data.list[[(i+1)]]
    }
    data.list= list.remove(data.list,length(data.list))
    
    #data.list = data.list[-which(lapply(data.list,is.null) == T)]
  } 
  sse <- c(sse, best$withinss[1], best$withinss[2])
  sse<-sse[-index] # will remove the second element of the  
}


cluster = list()
for(i in 1:k){
  cluster[[i]] <- as.numeric(rownames(data.list[[i]]))
}


finaldata.df <- originaldata.df[c(1)]
for(i in 1:nrow(data.df))
{
  if(is.element(finaldata.df[i,],cluster[[1]])){finaldata.df[i,] = 1} else if
  (is.element(finaldata.df[i,],cluster[[2]])){finaldata.df[i,] = 2}else if
  (is.element(finaldata.df[i,],cluster[[3]])){finaldata.df[i,] = 3}
}


finalvec <- unlist(finaldata.df)
finalvec <- unname(finalvec)
functionreturn = list(finalvec,sse)

print(functionreturn)

return(functionreturn)
}#end of function 1 braces

####################################################################################

#Code for comparing kmeans with result from bisecting kmeans
# Compare outcomes of kmeans with bisecting kmeans in terms of:
# 1. Overall SSE
# 2. Plot the clusters and compare the size and shape of clusters generated
# 3. Using the plot, we can also verify (visually) if points are being assigned to different clusters

# Input:
# data.df:  Dataset used for kmeans/bisecting kmeans clustering 
# Result: Variable of type list, obtained on running bkm() function
# k : k value for k-means
# km_centers: ID values of centers for KMeans

#Returns:
# Nothing, just print the observations requested

####################################################################################

kmeans_comparison <- function(data.df, result, k, km_centers){
  
data.df <- data.df[c(2:5)]
kmeans_out1 <- kmeans(data.df,k)
kmeans_out2 <-  kmeans(data.df,data.df[km_centers,])
kmeans_sse1 <- kmeans_out1$tot.withinss
kmeans_sse2 <- kmeans_out2$tot.withinss

bkm_sse <- unlist(bkm_result[2])
bkm_totsse = 0
for(i in 1:k){
  bkm_totsse <- bkm_totsse + bkm_sse[i] 
}

comparison <- matrix(c(kmeans_sse1, kmeans_sse2,bkm_totsse),ncol=3,byrow=TRUE)
colnames(comparison) <- c("Kmeans using K","Kmeans using km_centres","bisecting Kmeans")
rownames(comparison) <- c("SSE")
comparison <- as.table(comparison)
print(comparison)

#Plotting the clusters
plot_clustering_result(data.clustering, kmeans_out1, "KMeans using k outcome", k)
plot_clustering_result(data.clustering, kmeans_out2, "KMeans using km_centres outcome", k)
plot_clustering_result(data.clustering, bkm_result, "Bisecting KMeans outcome", clustering_k)
}#end of function 2 braces

####################################################################################


####################################################################################
# KNN Classifier
# implement my_knn with euclidean distance, majority vote and randomly resolving ties

# Input: 
# train: training data frame
# test: test data frame
# cl: class labels for training data
# k: 'k' value in KNN
# k value for KNN - Play around with different k values to see which 
# k gives best accuracy


# Output:
# A vector of class labels. return variable should be of type factor

####################################################################################

my_knn <- function(train, test, cl, k)
{
  
  distance =  matrix(nrow = 44, ncol= 106)
  for(i in 1:nrow(test))
  {
    for(j in 1:nrow(train))
    {
      l =1
      suma = matrix(nrow=1, ncol=4)
      while(l<5)
      {
        suma[l] <- ((test[i,l] - train[j,l])^2)
        l = l + 1
      }
      distance[i,j]  <- sqrt(suma[1]+suma[2]+suma[3]+suma[4])
    }
  }
  
  classified = matrix(nrow = 44, ncol= k)
  for (i in 1:nrow(distance))
  {
    sorted <- head(order(distance[i,]),k)
    for(j in 1: k){
      classified [i,j] <- sorted[j]
    }
  }
  
  labelled = matrix(nrow = 44, ncol= k)
  for(i in 1:nrow(classified)){
    for(j in 1:ncol(classified)){
      labelled [i,j] <- cl[classified [i,j]]
    }
  }
  
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  final = c()
  for(i in 1:nrow(labelled)){
    final[i] <- mode(labelled [i,])
  }
  
  for(i in 1:length(final)){
    if(final[i] == 1){final[i] = 'setosa'}else if(final[i] == 2)
    {final[i] = 'versicolor'}else if (final[i] == 3)
    {final[i] = 'virginica'}
  }
  final <- factor(final)
}#end of function 3 braces

####################################################################################
# Generate accuracy measures for your KNN classification
# Input:
# test_cl: actual class labels for test data
# knn_result: predicted class labels for test data

# Output:
# A vector of size 4 in the following order: 
# (overall accuracy, precision for the class 'setosa', recall for the class 'setosa', F-measure for the class 'setosa')
####################################################################################
my_knn_accuracy <- function(test_cl, knn_result){
  
  sum = 0
  mat <- table(knn_result,test_cl)
  for(i in 1:3){
    for(j in 1:3){
      if(i == j){sum = sum + mat[i,j]}
    }
  }
  
  
  acc <- sum/length(knn_result)
  
  tp <- mat[1]
  fp <- mat[2]+mat[3]
  fn <- mat[4]+mat[7]
  tn <- mat[5]+mat[6]+mat[8]+mat[9]
  
  conf_mat = matrix(nrow = 2, ncol = 2)
  conf_mat[1] = tp
  conf_mat[2] = fp
  conf_mat[3] = fn
  conf_mat[4] = tn
  
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  fmeasure <- (2*tp)/((2*tp)+fp+fn)
  
  acc_measures =c(acc,precision,recall,fmeasure)
  
  print(acc_measures)
  
}#end of function 4 braces

####################################################################################

# Functions to load data and plot data
# Load data for KMeans and KNN
load_data_clustering <- function(){
  data(iris)
  iris$ID <- c(1:nrow(iris)) # Create an id column for easy identification
  return(iris[,c('ID','Sepal.Length','Sepal.Width','Petal.Length', 'Petal.Width')])
}

load_data_classification <- function(){
  data(iris)
  # normalize all predictors, i.e., all but last column species
  iris[, -ncol(iris)] <- scale(iris[, -ncol(iris)])
  trainIdx <- createDataPartition(1:nrow(iris), p = 0.7, list = FALSE) # separate data into train, test (70, 30)
  testIdx <- setdiff((1:nrow(iris)), trainIdx)
  train <- iris[trainIdx, -ncol(iris)]
  test <- iris[testIdx, -ncol(iris)]
  train_cl <- factor(iris[trainIdx, ncol(iris)])
  test_cl <- factor(iris[testIdx, ncol(iris)])
  return(list(train, test, train_cl, test_cl))
}

################################
# Clustering
################################
# load the data for clustering
data.clustering <- load_data_clustering()

# k value for KMeans and Bisecting KMeans
# Play around with the clustering_k variable to see how bisecting kmeans behaves as k value changes
clustering_k <- 3

# run bisecting kmeans
bkm_result <- bkm(data.clustering, 25, clustering_k)

# plot bisecting kmeans result
plot_clustering_result(data.clustering, bkm_result, "Bisecting KMeans outcome", clustering_k)

# compare bisecting kmeans vs Kmeans
km_centers <- c(15,16,19)
kmeans_comparison(data.clustering, bkm_result, clustering_k, km_centers)


################################
# Classification
################################
# load the data for classification
data.classification <- load_data_classification()
train <- data.classification[[1]]
test <- data.classification[[2]]
train_cl <- as.factor(data.classification[[3]])
test_cl <- as.factor(data.classification[[4]])

# k value for KNN - Play around with different k values to see which 
# k gives best accuracy
classification_k <- 10

# run KNN classifier
knn_result <- my_knn(train, test, train_cl, classification_k)

# get the accuracy measures for K
my_knn_result <- my_knn_accuracy(test_cl, knn_result)

#end of program#

