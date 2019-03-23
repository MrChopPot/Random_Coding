# Install required packages for neural network
#install.packages(c("RCurl","nnet","scales","Ecdat"))

# Additional preparation for multi-layer neural network
#cran <- getOption("repos")
#cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
#options(repos = cran)
#install.packages("mxnet")

# Clear the environment if remaining
rm(list=ls())

# Set working directory
#setwd("xxx")

### Use a set of relevant library
library(Ecdat)
library(nnet)
library(scales)

### 1.Read Data

data("Catsup")
summary(Catsup)
head(Catsup,20)
nrow(subset(Catsup,price.heinz32 < 1))
nrow(subset(Catsup,price.heinz28 < 1))
nrow(subset(Catsup,price.hunts32 < 1))

### 2-0 Prepare training data and testing data
set.seed(1)
tr = sample(1:nrow(Catsup),2000)

### 2-1 Neural net with 5 hidden variables
set.seed(1)
n.net <- nnet(choice ~ .-id, data=Catsup[tr,], size=5, decay=0.006, linout=T, skip=T, maxit=3000, Hess=T)
n.net
summary(n.net)

# predict using test data
pred = predict(n.net, newdata=Catsup[-tr,], type="class") # pay attention to the type
tab = table(Catsup$choice[-tr], as.factor(pred))
tab2 = tab
tab2[,1]=tab[,3] # just change elements
tab2[,3]=tab[,1] # just change elements
colnames(tab2)=colnames(tab2)[c(3,2,1,4)] # change the position of colnames
tab2
cat(paste(
  "Accuracy:",
  round((sum(diag(tab2)))  /  sum(tab2),3)
 ) )
# Accuracy = 0.649

# Import function to display fitted network structure
library(RCurl)
root.url<-'https://gist.githubusercontent.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  sep='/'
)
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')

# Display fitted network structure
par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot(n.net)

### 2-2 Neural net with 20 hidden variables
set.seed(1)
n.net2 <- nnet(choice ~ .-id,data=Catsup[tr,],
              size=20,decay=0.006,linout=T,skip=T,
              maxit=6000,Hess=T)
pred = predict(n.net2,newdata=Catsup[-tr,],type="class")
tab = table(Catsup$choice[-tr],as.factor(pred))
tab2 = tab
tab2[,1]=tab[,3]
tab2[,3]=tab[,1]
colnames(tab2)=colnames(tab2)[c(3,2,1,4)]
tab2
cat(paste(
  "Accuracy:",
  round((sum(diag(tab2)))  /  sum(tab2),3)
))

# Accuracy = 0.652 when # of hidden variables is 15
# Accuracy = 0.683 when # of hidden variables is 20
# Accuracy = 0.678 when # of hidden variables is 30
# Accuracy = 0.693 when # of hidden variables is 40
# Accuracy = 0.680 when # of hidden variables is 50

# Display fitted network structure
par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot(n.net2,nid=F)


### 3-1 Multi-layer Neural network with  hidden variables
library(mxnet)

# 3-1-1 Prepare variables
set.seed(1)
tr = sample(1:nrow(Catsup),2000)
train<-data.matrix(Catsup[tr,])
test<-data.matrix(Catsup[-tr,])
train.x<-train[,-c(1,ncol(train))]
train.y<-as.factor(train[,ncol(train)])
train.x<-t(train.x)
test_org<-test
test<-test[,-c(1,ncol(test))]
test<-t(test)
table(train.y)

# 3-1-2. build Deep Neural Network (DNN)
data  <- mx.symbol.Variable("data")
fc1   <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1  <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
#drop1 <- mx.symbol.Dropout(act1, p=0.2)
fc2   <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2  <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
#drop2 <- mx.symbol.Dropout(act2, p=0.2)
fc3   <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=8)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")
devices <- mx.cpu()
mx.set.seed(0)

# 3-1-3. Perform DNN
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                     ctx=devices, num.round=100, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))
graph.viz(model$symbol)

## 3-1-4. Predict outcome 
preds <- predict(model,test, ctx=devices)
dim(preds)
pred.label <- max.col(t(preds)) - 1
table(pred.label)
head(pred.label)
table(test_org[,ncol(test_org)],pred.label)
sum(diag(table(test_org[,ncol(test_org)],pred.label)))/(nrow(Catsup)-2000)  ## accuracy for test data

# 128 64 8 Without dropouts 10000 iters, 0.6666667
# 128 64 8 Without dropouts  3000 iters, 0.6754386
# 128 64 8 Without dropouts  3000 iters, 0.6691729
# 128 64 8 Without dropouts  2000 iters, 0.6553885 
# 128 64 8 Without dropouts  1000 iters, 0.6541353 
# 128 64 8 Without dropouts   500 iters, 0.6528822
# 128 64 8 Without dropouts   300 iters, 0.6516291
# 128 64 8 Without dropouts   200 iters, 0.6453634
# 128 64 8 Without dropouts   100 iters, 0.6441103
# 128 64 8 Without dropouts    50 iters, 0.622807

# 3-1-2. Deep Neural Network (DNN) with four hidden layers
data  <- mx.symbol.Variable("data")
fc1   <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=256)
act1  <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
#drop1 <- mx.symbol.Dropout(act1, p=0.2)
fc2   <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=128)
act2  <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
#drop2 <- mx.symbol.Dropout(act2, p=0.2)
fc3   <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=64)
act3  <- mx.symbol.Activation(fc3, name="relu3", act_type="relu")
#drop3 <- mx.symbol.Dropout(act3, p=0.2)
fc4   <- mx.symbol.FullyConnected(act3, name="fc4", num_hidden=8)
softmax <- mx.symbol.SoftmaxOutput(fc4, name="sm")
devices <- mx.cpu()
mx.set.seed(0)

# 3-1-3. Perform DNN
model2 <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,
                                     ctx=devices, num.round=1000, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))
graph.viz(model2$symbol)

## 3-1-4. Predict outcome 
preds2 <- predict(model2,test, ctx=devices)
dim(preds2)
pred2.label <- max.col(t(preds2)) - 1
table(pred2.label)
head(pred2.label)
table(test_org[,ncol(test_org)],pred2.label)
sum(diag(table(test_org[,ncol(test_org)],pred2.label)))/(nrow(Catsup)-2000)  ## accuracy for test data

# 256 128 64  8 Without dropouts 1000 iters,  0.6528822
# 256 128 64 10 Without dropouts 1000 iters,  0.660401
# 256 128 64 10 With    dropouts 1000 iters,  0.6541353


# 4. Random Forest

## 4-0. Prepare data sets
tr2    = Catsup[ tr,]
te2    = Catsup[-tr,]

library(randomForest)
test_RF <- data.matrix(Catsup[-tr,])
test_RF2 = Catsup[-train]

set.seed(1)
result_RF1 = randomForest(train.y~.-choice -id,data=train,mtry=5,importance=TRUE) ## RF
result_RF1 

yhat_RF1.result = predict(result_RF1,test_RF,type="class")
yhat_RF1.table = table(te2$choice,yhat_RF1.result)
print("==== Class")
print(yhat_RF1.table)

# Evaluate the quality of prediction
Accuracy_RF = (sum(diag(yhat_RF1.table)))/nrow(te2)
print(Accuracy_RF)
# Accuracy: 0.7180451


# 5. Classification tree
library(rpart)      # Library for decision tree

# Fit classification tree
result = rpart(train.y~.-choice -id, data=tr2, method="class", parms=list(split="gini")) # Implement CART
result

library(rpart.plot) # Library for plotting a tree
prp(result, type=3, extra=102, nn=TRUE, fallen.leaves=TRUE, faclen=0, varlen=0, shadow.col ="grey")

# Check the prediction performance using Testing Data
# We do so by constructing a confusion matrix
# To measure how the fitted model performs against unknown & new data, 
# we predict the outcome using Testing Data that we did not use for fitting
yhat.result = predict(result,te2,type="class") 
yhat.table  = table(te2$choice,yhat.result) # Construct the confusion matrix 
print("==== Class")
print(yhat.table) # Display the confusion matrix

# Evaluate the quality of prediction
Accuracy_ctree  = (sum(diag(yhat.table)))/nrow(te2)
print(Accuracy_ctree) # 0.6340852


# 6. Multinominal logistics regression 

# Perform multinominal logistic regression

glm.fit=multinom(train.y~.-choice -id, data=tr2)
summary(glm.fit)
yhat_MNL.result = predict(glm.fit,te2,type="class")

# Display the confusion matrix
yhat_MNL.table  = table(te2$choice,yhat_MNL.result) # Construct the confusion matrix 
print("==== Class")
print(yhat_MNL.table) 

# Evaluate the quality of prediction
Accuracy_MNL  = (sum(diag(yhat_MNL.table)))/nrow(te2)
print(Accuracy_MNL) # 0.6478697

