rm(list=ls())

library(stringi)
library(digest)
#devtools::install_github("hadley/lineprof")
library(lineprof)

source("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Predicting_Parkinsons_Disease_Status\\RFtesting.R")

setwd("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Parkinsons data\\5 tests")
#setwd("C:/Users/Tri/Documents/")

## classic iris example
data(iris)

#### another example data
# dt <- readingSkills[c(1:105),]
# label <- "nativeSpeaker"

#### Iris example, training
x2 <- buildTree(iris, "Species", min_instance = 1, split_measure = 20, max_depth = 10, info_gain = 0.1, numOfFeatures=4)

print_tree(x2) 
accComp(dt, x2, label)

###### time testing
a <- Sys.time()
x2 <- buildTree(iris, "Species", min_instance = 1,
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
forest_iris=buildForest(4, iris, "Species", numOfFeatures=4)
fp_iris=forestPredict(iris, forest_iris)
accuracy(iris, "Species", fp_iris)

#### project data
dt=read.csv("roch_all_data.csv")

## Data cleaning
delete_index <- rep_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- inf_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- na_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
dt <- dt[-c(1,  ncol(dt))] ## Exclude file name and other useless information

label <- "Status"

Rprof("build_tree_profile_jit")
x3 <- buildTree(dt, label, min_instance = 1, max_depth = 5, info_gain = 0.001) 
#Rprof(NULL)  #some code NOT to be profiled could be added below
Rprof()
summaryRprof("build_tree_profile_jit")
print_tree(x3)

Rprof("accComp_profile_jit")
accComp(dt, x3, label)
Rprof()
summaryRprof("accComp_profile_jit")


l <- lineprof(buildTree(dt, label, min_instance = 1, max_depth = 5, info_gain = 0.001))
l
shine(l) 

Rprof("buildForest_profile_jit")
set.seed(12-4-16)
forest=buildForest(4, dt, label, info_gain = 0.001)
Rprof()
summaryRprof("buildForest_profile_jit")

Rprof("forestPredict_profile_jit")
fp=forestPredict(dt, forest)
#Error in matrix(unlist(value, recursive = FALSE, use.names = FALSE), nrow = nr,  : 
#length of 'dimnames' [2] not equal to array extent
#err @ tree 3
#err @ obs 4019
accuracy(dt, label, fp)
Rprof()
summaryRprof("forestPredict_profile_jit")

##debug
prediction(dt[4018,], forest[[3]]) #ok
prediction(dt[4019,], forest[[3]]) #throws error

### Paramemers setting

label <- "Status"
split_measure <- 15  ## how many cutoffs points are used in optimization
max_depth <- 5
min_instance <- 1
info_gain <- 0.001

#buildTree(dt, label, min_instance = 1, max_depth = 10, info_gain = 0.001)
