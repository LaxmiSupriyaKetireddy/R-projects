### Name: Laxmi Supriya Ketireddy
### NetID: LXK170003   

library(tidyverse)
library(broom)
library(data.table)

#setwd(???c:/data/BUAN6357/HW_2???); source(???prep.txt???, echo=T)

getwd()
setwd("C:/Users/Supriya/Documents/Sem 4/AdvancedBAwithRHW/HW_2")
load("C:/Users/Supriya/Documents/Sem 4/AdvancedBAwithRHW/HW_2/HW_2_data.RData")

#train <- read.csv(file="HW_2_train.csv",header=TRUE, sep=",")
#test <- read.csv(file="HW_2_test.csv",header=TRUE, sep=",")

train <- as.data.table(train)
test <- as.data.table(test)
head(train)
head(test)



# parameterization
minClusters        <- 1
maxClusters        <- 10

byRows             <- 1  # for use with apply()
byCols             <- 2  # for use with apply()

seed               <- 661641  # for demo purposes ONLY!


# initialize RNG
set.seed(seed)
kmTWSS      <- rep(-1,maxClusters)   # initialize for subsequent sanity check
kmTWSS

#to estimate the number of clusters for kmeans() clustering on training and testing data sets
for (k in minClusters:maxClusters){ 
  set.seed(seed)
  kmTWSS[k]  <- kmeans(select(train,v1,v2,v3,v4,v5), k, nstart=5)$tot.withinss
  print(kmTWSS[k]) 
  # prints all the values of total within sum of sqaures
}

#nstart gives the smallest penalty function and this tells the best within scores for the first five vals

# elbow plot, which shows that the optimal number of clusters=3 i.e k=3
plot(minClusters:maxClusters, kmTWSS) 

# function for first-order differencing and scaling 
d1Calc        <- function(v) {
  n       <- length(v)
  d1      <- v[1:(n-1)] - v[2:n]         # first order differences
  d1scale <- d1/max(d1)                  # relative scale
  return (list(d1      = d1,
               d1scale = d1scale) )
}

(kmTd       <- d1Calc(kmTWSS) )  
# It is observed that there are big changes at 3 for d1Scale
# Hence concluding that k=3.

#Performing Kmeans on training set 
kmclust_train <- kmeans(train,3)
kmclust_train

#results on training set
print(kmclust_train$cluster)
print(kmclust_train$size)
print(kmclust_train$tot.withins)

#Using Cbind, it can be analyzed which data falls into which cluster in training set
c_train_mem <- cbind(kmclust_train$cluster)
c_train_mem


#Performing Kmeans on test data set 
kmclust_test <- kmeans(test,3, nstart=5)
kmclust_test
#results on testing set
print(kmclust_test$cluster)
print(kmclust_test$size)
print(kmclust_test$tot.withins)

###Using Cbind, we analyze which data falls into which cluster in testing set
c_test_mem <- cbind(kmclust_test$cluster)
c_test_mem


#----------------------------------------------------
# Hclustering 
#----------------------------------------------------

## Hclust for training Data
set.seed(seed)
dist_euc <- (dist(train,method = "euclidean"))^2

hclust_train <- hclust(dist_euc,method="complete")
hclust_train
plot(hclust_train)

wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)}

#function used for finding the membership and wss
hc_mem <- function(i, hc, x) {
  ct <- cutree(hc, i)
  sp <- split(x, ct)
  wss <- sum(sapply(sp, wss))
  wss}

hcTWSS <- rep(-1,maxClusters)

for ( i in 1:maxClusters){
  hcTWSS[i] <- hc_mem(i,hclust_train,train)}
hcTWSS

#plotting the the total withiness error sum squares
plot(hcTWSS)
# It can be seen that 3 clusters is the optimal number of clusters.

# drawing red borders around the 3 clusters on the dendogram
plot(hclust_train)
rect.hclust(hclust_train, k = 3, border = "red") 


## Applying Hclust for Test Data
dist_euc_test <- (dist(test,method = "euclidean"))^2

hclust_test <- hclust(dist_euc_test ,method="complete")
hclust_test
plot(hclust_test)


# draw the blue borders around the 3 clusters on the dendogram
rect.hclust(hclust_test, k = 3, border = "blue") 

hctest_final = cutree(hclust_test, k=3)
hctest_final
plot(hctest_final)



#source(???validate.txt???, echo=T)