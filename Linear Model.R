
setwd("c:/data/BUAN6357/HW_4");source("prep.txt", echo=T)
library(data.table)
library(broom)
library(tidyverse)
library(partykit)


seed <- 436604030
set.seed(seed)
airqaul <- data.table(airquality)
airqaul
# dropping the missing values rows
air <- na.omit(airqaul)

# dropping "Day" column
air$Day = NULL       

# factor the "Month" column
air$Month <- as.factor(air$Month)   

# Deliverable 2
raw <- air            

# Deliverable 3 - non-CV OLS model using raw
base.model <- lm(Ozone~., data=raw)    
summary(base.model)

# Deliverable 4 - 10% validation set index values
n <- nrow(raw)
tst <- sample(n,ceiling(n*0.1)) 


test <- raw[tst, ]
train <- raw[-tst, ]

# Deliverable 5 - OLS model using simple CV
cv.model <- lm(Ozone~., data=train)    

summary(cv.model)

# Deliverable 6 - tst fitted values from cv.model
cv.fitted <- predict(cv.model,raw)[tst]        
cv.fitted

#  Deliverable 7 - tst residuals from simple CV
cv.resid <- data.table(loc=1:length(tst), diff=1:length(tst))
cv.resid$loc <- tst

cv.resid$diff <- raw$Ozone[tst] - cv.fitted
cv.resid  

#loocv function
loocv <- function(train.set, model){
  set.seed(seed)
  n <- nrow(train.set)
  t <- 1:n
  idx <- sample(t, length(t))
  nfolds <- cut(seq(1,n), breaks = n, labels = FALSE)
  loocv.predicted <- c()
  for(i in idx){
    testIndexes <- which(nfolds==i, arr.ind = TRUE)
    test_data <- train.set[testIndexes,]
    train_data <- train.set[-testIndexes,]
    
    lm.model <- lm(model,train_data)
    predicted_vals <- predict(lm.model,test_data)
    loocv.predicted[i] <- predicted_vals
  }
  return(loocv.predicted)
}

# Deliverable 8 - validation residuals from LOOCV
jk.resid <- NULL
n        <- nrow(raw)
loocv.predicted <- loocv(raw, Ozone~.)
jk.resid$loc <- 1:n
jk.resid$diff <- raw$Ozone - loocv.predicted
jk.resid     <- as.data.table(jk.resid)
jk.resid




# KF model
n  <- nrow(raw)
k  <- 10
t <- rep( 1:k, each=ceiling(n/k) )
t <- t[1:n]
set.seed(seed)
idx  <- sample(n)
(t2 <- data.frame(t,idx))

#Deliverable 9 - validation residuals from K-fold
kf.resid <- data.table(k= 1:nrow(raw),loc=1:nrow(raw), diff=1:nrow(raw))
kf.resid$k <-t


kf_resid_tmp <- rep(0,ceiling(n/k))
gen_kf <- function(x) {
  kf_resid_tmp <- c()
  for (i in 1:x){
    kf.model<- lm(Ozone ~ Solar.R + Wind + Temp + Month,data =raw[t2[t!=i,]$idx,] )
    #kf.model<- lm(Ozone ~., data = raw)
    kf_resid_tmpi <- raw$Ozone[t2[t==i,]$idx] - predict(kf.model,raw)[t2[t==i,]$idx]
    kf_resid_tmp <- c(kf_resid_tmp,kf_resid_tmpi)
  }
  return(kf_resid_tmp)
}

str(gen_kf(10))

kf.resid$diff <-gen_kf(10)
kf.resid$k <- t2$t
kf.resid$loc <- t2$idx

kf.resid

source("validate.txt", echo=T)



