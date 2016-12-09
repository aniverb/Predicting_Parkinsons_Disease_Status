rm(list=ls())
library(lazyeval)
library(dplyr)
library(magrittr)
library(foreach)
library(doParallel)
library(party)

library(stringi)
library(digest)
#devtools::install_github("hadley/lineprof")
library(lineprof)
source("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Predicting_Parkinsons_Disease_Status\\RFtesting.R")

cl<-makeCluster(3)
registerDoParallel(cl)
clusterCall(cl, function() {library(lazyeval); library(dplyr); library(magrittr); library(foreach); library(doParallel); library(party)})

setwd("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Parkinsons data\\5 tests")
#setwd("C:/Users/Tri/Documents/")

## classic iris example
data(iris)
dt <- iris
label <- "Species"

#### another example data
# dt <- readingSkills[c(1:105),]
# label <- "nativeSpeaker"

#### Iris example, training
x2 <- buildTree(dt, label, min_instance = 1, split_measure = 20, max_depth = 10, info_gain = 0.1, numOfFeatures=4)

print_tree(x2) 
accComp(dt, x2, label)

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
summaryRprof("build_tree_profile_jit")

Rprof("accComp_profile_jit")
accComp(dt, x3, label)
summaryRprof("accComp_profile_jit")


l <- lineprof(buildTree(dt, label, min_instance = 1, max_depth = 5, info_gain = 0.001))
l
shine(l) 

set.seed(12-4-16)
forest2=buildForest(4, dt, label, info_gain = 0.001)
fp=forestPredict(dt, forest2)
accuracy(dt, label, fp)

### Paramemers setting

label <- "Status"
split_measure <- 15  ## how many cutoffs points are used in optimization
max_depth <- 5
min_instance <- 1
info_gain <- 0.001

#buildTree(dt, label, min_instance = 1, max_depth = 10, info_gain = 0.001)
