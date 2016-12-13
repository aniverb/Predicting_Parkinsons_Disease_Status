source("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Predicting_Parkinsons_Disease_Status\\RFtesting.R")

library(pROC)
library(randomForest)

setwd("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Parkinsons data\\5 tests")
#setwd("C:/Users/Tri/Documents/")

##################################
#our data
##################################
dt <- read.csv("roch_all_data.csv")

## Data cleaning
delete_index <- rep_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- inf_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- na_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
dt <- dt[-c(1,  ncol(dt))] ## Exclude file name and other useless information

#to avoid java related rounding errors
ddt <- apply(dt, 2, scale)
ddt <- apply(dt, 2, round, digits = 5)
ddt[, 1] <- dt[, 1]
dt <- ddt
dt <- as.data.frame(dt)

#spliting data into train, dev, and test sets
set.seed(12-4-16)
train_index <- createDataPartition(1:nrow(dt), 0.5)[[1]]
other <- seq(1, nrow(dt))[-train_index]
test_index <- sample(other, length(other) * 0.6)
dev_index <- other[(other %in% test_index) == FALSE]

train <- dt[train_index, ]
dev <- dt[dev_index, ] 
test <- dt[test_index, ]
label <- "Status"

#our implementation
a <- Sys.time()
set.seed(12-4-16)
testForest=buildForest(500, train, label, info_gain = 0.0001)#19 feat rule of thumb
b <- Sys.time()
testFp=forestPredict(test, testForest)
c <- Sys.time()
acc=accuracy(test, label, testFp) 
bf_time=b-a
fp_time=c-b
compImpo(testForest)[1:10,]

#package
set.seed(12-4-16)
packageForest=randomForest(factor(Status)~., train, replace=TRUE, ntree=500, importance=TRUE) #no depth/maxnodes
summary(packageForest)
packagePredict=predict(packageForest, dev)
accuracy(dev, label, packagePredict)
packagePredictT=predict(packageForest, test)
#packagePredictTP=predict(packageForest, test, type = 'prob')
accuracy(test, label, packagePredictT)

#most imp vars
imp=packageForest$importance
index=sort(imp[,'MeanDecreaseGini'], decreasing = T)
index[1:10]
'amp_mean move_meanTKEO        stay_s            mi       stay_En    e_dfc.Gait     pitch_dfa 
18.09748      15.64479      15.63928      14.26490      13.36729      13.23928      13.17342 
y_dfc.Gait         pitch      move_std 
13.16617      12.97735      12.94504'

#testlabBin=as.numeric(test[,1]==2)
#packagePredictTBin=as.numeric(packagePredictT==2)
#save(test, testlabBin, packagePredictT, packagePredictTP, file="ROCRobjects")

plot(roc(test$Status, as.numeric(packagePredictT)), col="red", main="ROC Using Package on Test Data")
plot(roc(test$Status, as.numeric(testFp)), col="red", main="ROC of our implementation")