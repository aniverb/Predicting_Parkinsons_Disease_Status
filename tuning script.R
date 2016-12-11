#### project data
#source("C:/Users/Tri/Documents/GitHub/Predicting_Parkinsons_Disease_Status/RFtesting.R")
source("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Predicting_Parkinsons_Disease_Status\\RFtesting.R")

library(randomForest)

## classic iris example
data(iris)
dt <- iris
label <- "Species"

#### another example data
# dt <- readingSkills[c(1:105),]
# label <- "nativeSpeaker"

#### Iris example, training
x2 <- buildTree(dt, label, min_instance = 1,
                split_measure = 20, max_depth = 10,
                info_gain = 0.1, numOfFeatures=4)

#x2 <- buildTree(iris, "Species", numOfFeatures = 4)
print_tree(x2) 
accComp(iris, x2, "Species")

###### time testing
a <- Sys.time()
x2 <- buildTree(dt, label, min_instance = 1,
                split_measure = 20, max_depth = 10, info_gain = 0.1, numOfFeatures=4)
b <- Sys.time() - a
b
######

### Single prediction
cat(prediction(iris[1,], x2), (iris[1, ]$Species) %>% as.character, "\n")

### Display all iris example predictions
for (i in 1:nrow(iris)) {
  cat(prediction(iris[i,], x2), (iris[i, ]$Species) %>% as.character, "\n")
}


set.seed(12-4-16)
forest=buildForest(4, iris, "Species", numOfFeatures=4)
fp=forestPredict(iris, forest)
accuracy(iris, "Species", fp)

### calculate importance
irisRF <- buildForest(10, iris, "Species", info_gain = 0.1,
                      numOfFeatures = 4)
compImpo(irisRF)
##################################

setwd("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Parkinsons data\\5 tests")
dt <- read.csv("roch_all_data.csv")

## Data cleaning
delete_index <- rep_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- inf_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- na_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
dt <- dt[-c(1,  ncol(dt))] ## Exclude file name and other useless information

#ddt <- apply(dt, 2, scale)
#ddt[, 1] <- dt[, 1]
#dt <- ddt
#dt <- as.data.frame(dt)

set.seed(12-4-16)
train_index <- createDataPartition(1:nrow(dt), 0.5)[[1]]
other <- seq(1, nrow(dt))[-train_index]
test_index <- createDataPartition(1:length(other), 0.3)[[1]]
dev_index <- seq(1, length(test_index))[-test_index]

train <- dt[train_index, ]
dev <- dt[dev_index, ] #alternative to cv
test <- dt[test_index, ]
label <- "Status"

#dt <- dt[train_index, c(1,seq(33, 186))]
a <- Sys.time()
set.seed(12-4-16)
tuneForest=buildForest(10, train, label, max_depth = 8, info_gain = 0.001)#19 feat rule of thumb, depth is another param to tune 15 too high, 5 too low
#Tree 11
# Error in .jcall("weka/filters/Filter", "Lweka/core/Instances;", "useFilter",  : 
#   java.lang.IllegalArgumentException: A nominal attribute (x_mean.Gait) cannot have duplicate labels ('(-0--0]').

#prediction(dev[1,], tuneForest[[2]])

b <- Sys.time()
tuneFp=forestPredict(dev, tuneForest)
c <- Sys.time()
tuneAcc=accuracy(dev, label, tuneFp) #0.5906702, .51 with info gain.0001, tree size .1
bf_time=b-a# 6.924353
fp_time=c-b
# tree #: 2 
# obs #: 1 
# Error in matrix(unlist(value, recursive = FALSE, use.names = FALSE), nrow = nr,  : 
#                   length of 'dimnames' [2] not equal to array extent
compImpo(tuneForest)[1:10,]

#tuning with package
a <- Sys.time()
packageForest=randomForest(Status~., train, type=classification, replace=TRUE, mtry=8, ntree=500, importance=TRUE) #no depth/maxnodes
b <- Sys.time()
summary(packageForest)
packagePredict=predict(packageForest, dev)
c <- Sys.time()
bf_time=b-a# 6.924353
fp_time=c-b




