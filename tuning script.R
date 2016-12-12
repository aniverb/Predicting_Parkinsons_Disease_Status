#### project data
#source("C:/Users/Tri/Documents/GitHub/Predicting_Parkinsons_Disease_Status/RFtesting.R")
source("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Predicting_Parkinsons_Disease_Status\\RFtesting.R")
#setwd("C:/Users/Tri/Documents")
setwd("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Parkinsons data\\5 tests")
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
#test_index <- createDataPartition(1:length(other), 0.3)[[1]]
#dev_index <- seq(1, length(test_index))[-test_index]

train <- dt[train_index, ]
dev <- dt[dev_index, ] #alternative to cross valid
test <- dt[test_index, ]
label <- "Status"

#dt <- dt[train_index, c(1,seq(33, 186))]
a <- Sys.time()
set.seed(12-4-16)
tuneForest=buildForest(10, train, label, info_gain = 0.001)#19 feat rule of thumb, depth is another param to tune 15 too high, 5 too low
#Tree 11
# Error in .jcall("weka/filters/Filter", "Lweka/core/Instances;", "useFilter",  : 
#   java.lang.IllegalArgumentException: A nominal attribute (x_mean.Gait) cannot have duplicate labels ('(-0--0]').

#prediction(dev[1,], tuneForest[[2]])

b <- Sys.time()
tuneFp=forestPredict(dev, tuneForest)
c <- Sys.time()
tuneAcc=accuracy(dev, label, tuneFp) # 0.5795007
bf_time=b-a# 6.924353
fp_time=c-b
# tree #: 2 
# obs #: 1 
# Error in matrix(unlist(value, recursive = FALSE, use.names = FALSE), nrow = nr,  : 
#                   length of 'dimnames' [2] not equal to array extent
compImpo(tuneForest)[1:10,]

#CV/tuning with package
a <- Sys.time()
set.seed(12-4-16)
rfCV=rfcv(dev[,2:ncol(dev)], factor(dev$Status), step=1, log=F, cv.fold=10, ntree=500)
b <- Sys.time()
1-min(rfCV$error.cv)
which(rfCV$error.cv==min(rfCV$error.cv))
packageForest=randomForest(factor(Status)~., train, replace=TRUE, ntree=500, importance=TRUE) #no depth/maxnodes
b <- Sys.time()
summary(packageForest)
packagePredict=predict(packageForest, dev)
c <- Sys.time()
bf_time=b-a# 6.924353
fp_time=c-b
accuracy(dev, label, packagePredict)# 0.614532
packagePredictT=predict(packageForest, test)
accuracy(test, label, packagePredictT)#.6040526

#most imp vars
imp=packageForest$importance
index=sort(imp[,'MeanDecreaseGini'], decreasing = T,index.return=T)
index$x[1:10]
'     amp_mean move_meanTKEO        stay_s            mi       stay_En     pitch_dfa 
     17.06559      15.98908      15.96468      14.12819      13.47229      13.10186 
     stay_iqr        stay_k      move_std    e_dfc.Gait 
     12.68971      12.54182      12.50310      12.39555 '
