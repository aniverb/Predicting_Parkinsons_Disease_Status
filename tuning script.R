#### project data
source("C:/Users/Tri/Documents/GitHub/Predicting_Parkinsons_Disease_Status/RFtesting.R")
dt <- read.csv("roch_all_data.csv")

## Data cleaning
delete_index <- rep_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- inf_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- na_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
dt <- dt[-c(1,  ncol(dt))] ## Exclude file name and other useless information

set.seed(12-4-16)
train_index <- createDataPartition(1:nrow(dt), 0.5)[[1]]
other <- seq(1, nrow(dt))[-train_index]
test_index <- createDataPartition(1:length(other), 0.3)[[1]]
dev_index <- seq(1, length(test_index))[-test_index]

dt <- dt[train_index, ]
label <- "Status"

#dt <- dt[train_index, c(1,seq(33, 186))]

set.seed(12-4-16)
forest2=buildForest(5, dt, label, info_gain = 0.0001, numOfFeatures = 5)
fp=forestPredict(dt, forest2)
accuracy(dt, label, fp)

##################################

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


