# Name: Laxmi Supriya Ketireddy
# NetId: LXK170003  


library(tidyverse)
library(data.table)
require(partykit)

setwd("c:/data/BUAN6357/HW_3")
source("prep.txt", echo=T)


#constants
cols      <- 7
byRows    <- 1
byCols    <- 2
seed      <- 211754294

#parametrs
n    <- 25
n1   <- 50  
p   <- 0.9

#Flags
debug   <- T
plots   <- T
verbose <- F
demo3   <- T

# to help reading of code
classes   <- c(0,1,2,3,4,5,6,7,8,9)
minDigit  <- min(classes)
numDigits <- length(classes)
maxDigit  <- max(classes)

# initialize
set.seed(seed)

# digits (copies of each digit)
t1 <- rep(classes, n)
#t1
summary(t1)
# segment patterns for each digit
t2 <- c(1,1,1,0,1,1,1,
        0,0,1,0,0,1,0,
        1,0,1,1,1,0,1,
        1,0,1,1,0,1,1,
        0,1,1,1,0,1,0,
        1,1,0,1,0,1,1,
        0,1,0,1,1,1,1,
        1,0,1,0,0,1,0,
        1,1,1,1,1,1,1,
        1,1,1,1,0,1,0)

t3 <- rep(t2, n)      # 25 copies of 7segments           
t4 <- rbinom(length(t3), 
             1,  # how many samples to be gen
             1-p)     # generate the errors randomly    # probability of failure event (noise)

# flip the bits to add noise [ t4 == 1 designates failure event ]
t5 <- ifelse(t4 == 1, 
             1-t3, 
             t3)

# reshape
t5                  <- matrix(data=t5, 
                              nrow=length(classes)*n, 
                              ncol=cols, 
                              byrow=T)

#t5
dim(t5)
dim(t1)             <- c(length(t1), 1)

t6                  <- cbind(t1, t5)
#t6
simDigits           <- as.data.table(t6)
#simDigits

colnames(simDigits) <- c("digit", "s1", "s2", "s3", "s4", "s5", "s6", "s7")

#fit classification models:
#--------------------------------------------------------------------------------
#Logistic Classification for 25 samples
#--------------------------------------------------------------------------------

#creating temp copy
td            <- simDigits
#td
fitted.logit  <- matrix(rep(NA,nrow(td)*numDigits),nrow=nrow(td) )
#fitted.logit


# save for later use (and re-use)
digits        <- td$digit
#digits
td$digit      <- NULL       

# fit: get predictions from individual models
for ( i in 1:length(classes) ) {
  d                 <- classes[i]
  td$y              <- 0         # initialize
  td$y[digits == d] <- 1         # indicator for -each- digit
  m                 <- glm(y ~ ., 
                           data=td, 
                           family=binomial())
  fitted.logit[,i]  <- m$fitted.values
}

if (debug) summary(m)

# classifying
index       <- apply(fitted.logit, 
                     byRows, 
                     which.max)   
#index  #location

logitClassif25 <- classes[index]

#scaling for normalization
scale.logit <- apply(fitted.logit, 
                     byRows, 
                     sum)     
#scale.logit

p.logit     <- apply(fitted.logit, 
                     byRows, 
                     max)/scale.logit   #normalizing the max

# Bayes Risk
logitBR25  <- 1-p.logit     #for the probability of failure event.
#logitBR25

if (debug) summary(scale.logit)

#table for comparing the actual and predicted 
(hits.logit <- table(logitClassif25, 
                     digits,
                     dnn=c("logitClassif25","actual") ) )
#percent correct
(pc.logit   <- sum(diag(hits.logit))/sum(hits.logit))

# cleanup
td$y        <- NULL
#td$y

# quick query
if (verbose) (td9 <- td[digits == 9, ])



#--------------------------------------------------------------------------------
#Multiple tree Classification for 25 samples
#--------------------------------------------------------------------------------

fitted.tree10 <- matrix(rep(NA,nrow(td)*numDigits), 
                        nrow=nrow(td) )

#get predictions from individual models
for ( i in 1:length(classes) ) {
  d25_1                  <- classes[i]
  td$y               <- 0         
  td$y[digits == d25_1]  <- 1         # indicator for each digit
  m              <- ctree(y ~ ., 
                              data=td)
  fitted.tree10[,i]  <- predict(m)
}
#fitted.tree10[,i]

#tree structure for last category.
#m

#plotting
if (plots) plot(m)

#cleanup
td$y <- NULL

#classify
index        <- apply(fitted.tree10, 
                      byRows, 
                      which.max)   
#index #location

tree10Classif25 <- classes[index]
#class.tree10

#scale for normalization
scale.tree10 <- apply(fitted.tree10, 
                      byRows, 
                      sum)
#scale.tree10

p.tree10     <- apply(fitted.tree10, 
                      byRows, 
                      max)/scale.tree10  #normalizing max value  
#p.tree10

# Bayes Risk
tree10BR25  <- 1-p.tree10   
#tree10BR25

if (debug) summary(scale.tree10)

#table to compare the classified and the actual values.
(hits.tree10 <- table(tree10Classif25, 
                      digits,
                      dnn=c("classif","actual") ) )
# Calculating the percent correct 
(pc.tree10   <- sum(diag(hits.tree10))/sum(hits.tree10))       


#--------------------------------------------------------------------------------
#Single tree Classification for 25 samples
#--------------------------------------------------------------------------------
# triggers classification
td$fDigits    <- as.factor(digits)  

m             <- ctree(fDigits~s1 + s2 + s3 + s4 + s5 + s6 + s7,
                       data=td)

fitted.tree1  <- predict(m)
#fitted.tree1

x             <- predict(m) 

tree1Classif25<- as.numeric(as.character(x))       
# individual class probabilities
p.tree1       <- predict(m,type="prob")   

# Checking if p.tree1 row is stochastic. Applying the condition if summary() is all 1 then: yes
if (debug) summary(apply(p.tree1, byRows, sum))

# find min Bayes Risk classification based on probabilities
mbr_tree1_25    <-   apply(p.tree1, 
                         byRows, 
                         which.max)
#tree1BR25
#Normalization
risk.tree1    <- 1-apply(p.tree1, 
                         byRows, 
                         max)

tree1BR25<-as.numeric(risk.tree1)
#Table for comparing classified and actual values.

(hits.tree1   <- table(tree1Classif25, 
                       digits,
                       dnn=c("classif","actual") ) )

# percent correct 
(pc.tree1 <- sum(diag(hits.tree1))/sum(hits.tree1))             

#boxplots
if (debug) {
  br.plots  <- data.frame(tree1BR25, 
                          tree10BR25, 
                          logitBR25)
  boxplot(br.plots,
          main="Bayes Risk by Classification Strategy",
          ylab="Bayes Risk")
  
  #summary(br.plots)
}

#Tree plot 
if (demo3) {
  td3           <- simDigits             # create temp copy
  td3           <- td3[ td3$digit < 3, ] # subset to digits 0, 1, 2 
  # (preserving original data)
  
  td3$digit     <- as.factor(td3$digit) # this should look familiar :-)
  
  m3_25            <- ctree(digit~., 
                         data=td3)
  
  plot(m3_25)      # note particularly the histograms in nodes 4 and 7 
  # (where all categories are present)
}

#########################################################################################
# For 50 samples
#########################################################################################
set.seed(seed)
# digits (copies of each digit)
t1_50 <- rep(classes, n1)
#t1_50
#summary(t1_50)
# segment patterns for each digit
t2_50<- c(1,1,1,0,1,1,1,
        0,0,1,0,0,1,0,
        1,0,1,1,1,0,1,
        1,0,1,1,0,1,1,
        0,1,1,1,0,1,0,
        1,1,0,1,0,1,1,
        0,1,0,1,1,1,1,
        1,0,1,0,0,1,0,
        1,1,1,1,1,1,1,
        1,1,1,1,0,1,0)

t3_50 <- rep(t2_50, n1)      # 50 copies of 7segments           
t4_50 <- rbinom(length(t3_50), 
             1,  # how many samples to be gen
             1-p)     # generate the errors randomly    # probability of failure event (noise)

# flip the bits to add noise [ t4 == 1 designates failure event ]
t5_50 <- ifelse(t4_50 == 1, 
             1-t3_50, 
             t3_50)

# reshape
t5_50                  <- matrix(data=t5_50, 
                              nrow=length(classes)*n1, 
                              ncol=cols, 
                              byrow=T)


#dim(t5_50)
dim(t1_50) <- c(length(t1_50), 1)

t6_50 <- cbind(t1_50, t5_50)

simDigits_50           <- as.data.table(t6_50)


colnames(simDigits_50) <- c("digit", "s1", "s2", "s3", "s4", "s5", "s6", "s7")

#fit classification models:
#--------------------------------------------------------------------------------
#Logistic Classification for 50 samples
#--------------------------------------------------------------------------------

#creating temp copy
td_50            <- simDigits_50
#td_50
fitted.logit_50  <- matrix(rep(NA,nrow(td_50)*numDigits),nrow=nrow(td_50) )


# save for later use (and re-use)
digits_50  <- td_50$digit
#digits_50
td_50$digit <- NULL       

# fit: get predictions from individual models
for ( i in 1:length(classes) ) {
  d_50                 <- classes[i]
  td_50$y              <- 0         # initialize
  td_50$y[digits_50 == d_50] <- 1         # indicator for each digit
  m                 <- glm(y ~ ., 
                           data=td_50, 
                           family=binomial())
  fitted.logit_50[,i]  <- m$fitted.values
}

if (debug) summary(m)

# classifying
index_50       <- apply(fitted.logit_50, 
                     byRows, 
                     which.max)   

logitClassif50 <- classes[index_50]

#scaling for normalization
scale.logit_50 <- apply(fitted.logit_50, 
                     byRows, 
                     sum)     


p.logit_50     <- apply(fitted.logit_50, 
                     byRows, 
                     max)/scale.logit_50   #normalizing the max

# Bayes Risk
logitBR50  <- 1-p.logit_50     #for the probability of failure event.
#logitBR50

if (debug) summary(scale.logit_50)

#table for comparing the actual and predicted 
(hits.logit_50 <- table(logitClassif50, 
                     digits_50,
                     dnn=c("classif","actual") ) )
#percent correct
(pc.logit_50   <- sum(diag(hits.logit_50))/sum(hits.logit_50))

# cleanup
td_50$y    <- NULL
#td_50$y

# quick query
if (verbose) (td9_50 <- td_50[digits_50 == 9, ])


#--------------------------------------------------------------------------------
#Multiple tree Classification for 50 samples
#--------------------------------------------------------------------------------

fitted.tree10_50 <- matrix(rep(NA,nrow(td_50)*numDigits), 
                        nrow=nrow(td_50) )

#get predictions from individual models
for ( i in 1:length(classes) ) {
  d_50                  <- classes[i]
  td_50$y               <- 0         
  td_50$y[digits_50 == d_50]  <- 1         # indicator for each digit
  m    <- ctree(y ~ ., 
                              data=td_50)
  fitted.tree10_50[,i]  <- predict(m)
}


#tree structure for last category.
m

#plotting
if (plots) plot(m)

#cleanup
td_50$y <- NULL

#classify
index_t50        <- apply(fitted.tree10_50, 
                      byRows, 
                      which.max)   


tree10Classif50 <- classes[index_t50]


#scale for normalization
scale.tree10_50 <- apply(fitted.tree10_50, 
                      byRows, 
                      sum)

p.tree10_50     <- apply(fitted.tree10_50, 
                      byRows, 
                      max)/scale.tree10_50  #normalizing max value  


# Bayes Risk
tree10BR50  <- 1-p.tree10_50   
#tree10BR50

if (debug) summary(scale.tree10_50)

#table to compare the classified and the actual values.
(hits.tree10_50 <- table(tree10Classif50, 
                      digits_50,
                      dnn=c("classif","actual") ) )
# Calculating the percent correct 
(pc.tree10_50   <- sum(diag(hits.tree10_50))/sum(hits.tree10_50))       


#--------------------------------------------------------------------------------
#Single tree Classification for 50 samples
#--------------------------------------------------------------------------------
# triggers classification
td_50$fDigits50    <- as.factor(digits_50)  

m <- ctree(fDigits50~.,
                        data=td_50)

fitted.tree1_50  <- predict(m)
x             <- predict(m)
# individual class probabilities
tree1Classif50<-as.numeric(as.character(x))          
p.tree1_50       <- predict(m,type="prob")  # individual class probabilities 
summary(apply(p.tree1_50, byRows, sum))

# Checking if p.tree1 row is stochastic. Applying the condition if summary() is all 1 then: yes
if (debug) summary(apply(p.tree1_50, byRows, sum))

# find min Bayes Risk classification based on probabilities
mbr.tree1    <-   apply(p.tree1_50, 
                        byRows, 
                        which.max)
#tree1BR50
#Normalization
risk.tree1_50    <- 1-apply(p.tree1_50, 
                         byRows, 
                         max)
#Table for comparing classified and actual values.
tree1BR50<-as.numeric(risk.tree1_50)

(hits.tree1_50   <- table(tree1Classif50, 
                       digits_50,
                       dnn=c("classif","actual") ) )

# percent correct 
(pc.tree1_50 <- sum(diag(hits.tree1_50))/sum(hits.tree1_50))             


#boxplots
if (debug) {
  br.plots_50  <- data.frame(tree1BR50, 
                          tree10BR50, 
                          logitBR50)
  boxplot(br.plots_50,
          main="Bayes Risk by Classification Strategy",
          ylab="Bayes Risk")
  
  summary(br.plots_50)
}

#Tree plot 
if (demo3) {
  td3_50           <- simDigits            # create temp copy
  td3_50           <- td3_50[ td3_50$digit < 3, ] # subset to digits 0, 1, 2 
  # preserving original data
  
  td3_50$digit_50     <- as.factor(td3_50$digit) # this should look familiar :-)
  
  m3_50            <- ctree(digit~., 
                            data=td3_50)
  
  plot(m3_50)      
  
}

source("validate.txt", echo=T)
