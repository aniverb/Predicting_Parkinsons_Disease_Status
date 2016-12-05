rm(list=ls())

library(lazyeval)
library(dplyr)
library(magrittr)
library(foreach)
library(doParallel)
cl<-makeCluster(3)
registerDoParallel(cl)

setwd("C:\\Users\\aniverb\\Documents\\Grad_School\\JHU\\475\\project\\Parkinsons data\\5 tests")

###################################################################
## Find inf pairs in the data
## input: a data set
## output: index of inf pairs
###################################################################

rep_find <- function(dt) {
  out <- NULL
  for (i in 1:(nrow(dt)-1)) {
    if (dt[i, 2] == dt[i+1, 2]) {
      if (dt[i, 2] == 1) {
        out <- c(out, i)
      } else {
        out <- c(out, i + 1) 
      }
    } 
  }
  return(out)
}

###################################################################
## Find inf pairs in the data
## input: a data set
## output: index of inf pairs
###################################################################

inf_find <- function(dt) {
  inf_dt <- sapply(dt, is.infinite)
  if (!any(inf_dt)) return(numeric(0))
  inf_index <- apply(inf_dt, 2, which)
  inf_index <- as.numeric(unlist(inf_index))
  if (length(inf_index) > 0) {
    pairs <- NULL
    for (q in seq_along(inf_index)) {
      if (inf_index[q] %% 2 == 0){
        pairs <- c(pairs, inf_index[q] - 1)
      } else {
        pairs <- c(pairs, inf_index[q] + 1)
      }
    }
    inf_index <- sort(unique(c(inf_index, pairs)))
  }
  return(inf_index)
}

###################################################################
## Find NA pairs in the data
## input: a data set
## output: index of NA pairs
###################################################################

na_find <- function(dt) {
  inf_dt <- apply(dt, 2, is.na)
  inf_index <- apply(inf_dt, 2, which)
  inf_index <- as.numeric(unlist(inf_index))
  if (length(inf_index) > 0) {
    pairs <- NULL
    for (q in seq_along(inf_index)) {
      if (inf_index[q] %% 2 == 0){
        pairs <- c(pairs, inf_index[q] - 1)
      } else {
        pairs <- c(pairs, inf_index[q] + 1)
      }
    }
    inf_index <- sort(unique(c(inf_index, pairs)))
  }
  return(inf_index)
}

###################################################################
## Compute Gini index
## input: a data set, colname of the label
## output: gini index
###################################################################

giniIndex <- function(dt, label) {
  gp_row <- count_(dt, label)[[2]]
  frac <- gp_row / sum(gp_row)
  1 - sum(frac ^ 2)
}

###################################################################
## Compute impurity/gini index for each group
## input: cutoff, number of the column and split data
## output: impurity
###################################################################

impuFun <- function(x, i, dt, label, col_name) {
  a1 <- filter_(dt, paste(col_name[i], ">", x)) %>% select_(., label) 
  a2 <- filter_(dt, paste(col_name[i], "<=", x)) %>% select_(., label) 
  nrow(a1) / nrow(dt) * giniIndex(a1, label) + 
    nrow(a2) / nrow(dt) * giniIndex(a2, label)
}

###################################################################
## split the data set into two groups
## input: cutoff, number of the column for criteria and data set
## output: a list of two groups
###################################################################

divideSet <- function(x, i, dt, col_name) {
  a1 <- filter_(dt, paste(col_name[i], "<=", x)) 
  a2 <- filter_(dt, paste(col_name[i], ">", x))
  return(list(a1, a2))
}

###################################################################
## Stopping criterion
## input: data frame and other parameters
## output: boolean logic (except badSplit(.), it outputs a value)
###################################################################

#### stop criteria 1: all instances have the same label

numLabels <- function(dt, label) {
  a1 <- select_(dt, label) %>% unique %>% nrow
  (a1 == 1)
}

#### stop criteria 2: reach maximum depth
maxDepth <- function(nn, mdep) {
  nn >= mdep
}

#### stop criteria 3: too few instances for child node
numEx <- function(dt, num) {
  nrow(dt) < num
}

#### stop criteria 4: split will not gain info
badSplit <- function(sdt1, sdt2, dt, label) {
  giniIndex(dt, label) - 
    nrow(sdt1) / nrow(dt) * giniIndex(sdt1, label) -
      nrow(sdt2) / nrow(dt) * giniIndex(sdt2, label)
}


###################################################################
## Create a leaf (terminal) node, whenever satisfies stopping criteria
## input: data frame 
## output: predicted label
###################################################################

leafNode <- function(dt, label) {
  a1 <- count_(dt, label)
  z1 <- which.max(a1[[2]]) 
  prediction <- a1[[1]][z1] %>% as.character
  return(prediction)
}

buildTree <- function(dt, label, min_instance = 1,
                      max_depth = 10, info_gain = 0.1,
                      n_now = 1, split_measure = 20, numOfFeatures=NA) {
  
  if (is.na(numOfFeatures)){
    numOfFeatures=floor(sqrt(ncol(dt)-1)) #-1 subtract label col
  }
  
  ### Step 0. check early-stopping criteria

  col_name <- colnames(dt)
  col_name <- col_name[-which(col_name == label)]
  col_name <- sample(col_name, numOfFeatures)
  
  cat("depth is:", n_now, "\n")
  
  if (numLabels(dt, label) | maxDepth(n_now, max_depth) |
     numEx(dt, min_instance)) {
    
    cat("Early stopping", "\n")
    return(leafNode(dt, label))
    
  } else {
    
    ### Step 1.
    ### Compute the best split, 
    ### use optim for time-saving 
    
    out <- NULL
    for (i in seq_along(col_name)) { 
      
      all_cut <- arrange_(dt, col_name[i]) %>% select_(., col_name[i]) %>% unique  ## get the value of i-th column
      cutoffs <- (all_cut[-1,] + all_cut[-length(all_cut),]) / 2
      
      num_split <- ifelse(split_measure < length(cutoffs),
                          split_measure, length(cutoffs))
      #cat(cutoffs, "\n")
      
      for (j in seq_len(num_split)) {
        
        # res <- optim(cutoffs[round(length(cutoffs) / j)], impuFun,
        #              label = label, i = i, dt = dt,
        #              col_name = col_name, method = "BFGS")
        
        res <- nlm(impuFun, cutoffs[round(length(cutoffs) / j)],
            label = label, i = i, dt = dt, col_name = col_name)

        out <- rbind(out, c(res[[1]], res[[2]], i, j))
        cat(res[[1]], res[[2]], i, j, "\n")
      }
    }
    
    # best_split <- which.min(out[, 2])[1]
    # best_cut <- out[best_split, 1]
    # best_col <- out[best_split, 3]

    best_split <- which.min(out[, 1])[1]
    best_cut <- out[best_split, 2]
    best_col <- out[best_split, 3]
    
        
    ### Step 2.
    ### Split the data
    
    divide <- divideSet(best_cut, best_col, dt, col_name)
    
    ### Step 3.
    ### check the subset has enough information gain
    cat("Info gain is:", badSplit(divide[[1]], divide[[2]], dt, label), "\n")
    
    if (badSplit(divide[[1]], divide[[2]], dt, label) < info_gain) {
      cat("Bad Split", "\n")
      return(leafNode(dt, label))
    }
    
    res <- list(column = best_col, cutoff = best_cut,
                L_tree = divide[[1]], R_tree = divide[[2]])

    ### Step final 
    ### grow subtrees
    
    v1 <- buildTree(res$L_tree, label, min_instance = 1, max_depth = 10,
                     n_now = n_now + 1)
    v2 <- buildTree(res$R_tree, label, min_instance = 1, max_depth = 10,
                     n_now = n_now + 1)
    
    tree <- list(column = col_name[best_col],
                cutoff = best_cut,
                depth = n_now,
                left_tree = v1,
                right_tree = v2)
    
    return(tree)
  }
}

###################################################################
## Make prediction
## input: a new instance, a trained model
## output: a prediction
###################################################################

prediction <- function(instance, model) {
  if (instance[model$column] < model$cutoff) {
    if (!is.list(model$left_tree)) {
      return(model$left_tree %>% as.character)
    } else {
      prediction(instance, model$left_tree)
    }
  } else {
    if (!is.list(model$right_tree)) {
      return(model$right_tree %>% as.character)
    } else {
      prediction(instance, model$right_tree)
    }
  }
}


## Iris trained model display

###################################################################
## Display trained tree/model
## input: a trained tree/model
## output: tree display
###################################################################

print_tree <- function(X, prefix = "", prefix_2 = " ") {
  for (i in c(1, 4, 5)) {
    if (i == 1) {
      cat(prefix, X[[i]], "<", round(X[[i + 1]], 3), "\n", sep = " ")
    } else if (!is.list(X[[i]]) & i == 4) {
      cat(prefix_2, "True : ", X[[i]], "\n", sep = " ")
    } else if (!is.list(X[[i]]) & i == 5) {
      cat(prefix_2, "False: ", X[[i]], "\n", sep = " ")
    } else {
      print_tree(X[[i]], paste0(prefix, " "), paste0(prefix_2, "  ")) 
    } 
  }
}


#calculating accuracy for random forrest
accuracy=function(dt, label, predictions){
  actual=dt[[label]] 
  accuracy=mean(predictions==actual)
  return(accuracy) 
}

#bootstrap data
bootstrap=function(dt){
  n=nrow(dt)
  id=sample(1:n, n, replace=T)
  bs_data=dt[id, ]
  return(bs_data)
}


buildForest=function(numOfTrees, dt, label, min_instance = 1, max_depth = 10, info_gain = 0.1, n_now = 1, split_measure = 15, numOfFeatures=NA){
  forest=list()
  for (i in 1:numOfTrees){
    dt=bootstrap(dt)
    forest[[i]] = buildTree(dt, label, min_instance, max_depth, info_gain, n_now, split_measure, numOfFeatures)
  }
  return(forest)
}


countVote=function(votes){
  counts=table(votes)
  ids=which(counts==max(counts))
  if (length(ids)>1){
    ids=ids[1]
  }
  maxVote=names(counts)[ids]
  return(maxVote)
  }


forestPredict=function(dt, forest){
  numtrees=length(forest)
  n=nrow(dt)
  predictionsMat=matrix(nrow=n, ncol=numtrees)
  id=0
  for (tree in forest){
    predictions=c()
      for (i in 1:nrow(dt)) {
        predictions[i]=prediction(dt[i,], tree)
      }
    id=id+1
    predictionsMat[,id]= predictions
  }
  maxVotes=apply(predictionsMat, 1, countVote)
  return(maxVotes)
}


###################################################################
## Compute accuracy for decision tree
## input: data frame to be predicted, a trained model, label
## output: accuracy (%)
###################################################################

accComp <- function(dt, model, label) {
  pred_label <- sapply(seq_len(nrow(dt)), function(x) prediction(dt[x,], model))
  true_label <- select_(dt, label)
  accu <- sum(pred_label == true_label) / nrow(dt)
  cat("Accuracy is:", accu * 100, "%")
}


###################################################################
###################################################################
###################################################################

## classic iris example
data(iris)
dt <- iris
label <- "Species"


#### Iris example, training
x2 <- buildTree(dt, label, min_instance = 1,
                split_measure = 20, max_depth = 10, info_gain = 0.001, numOfFeatures=4)
print_tree(x2) 
accComp(dt, x2, label)

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

x3 <- buildTree(dt, label, min_instance = 1, max_depth = 5, info_gain = 0.001) 
accComp(dt, x3, label)

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
