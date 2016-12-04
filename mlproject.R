rm(list=ls())

library(data.table)
library(dplyr)
library(magrittr)
library(lazyeval)
data(iris)

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
## Compute impurity
## input: cutoff, number of the column and data set
## output: impurity
###################################################################

impuFun <- function(x, i, dt) {
  a1 <- filter_(dt, paste(col_name[i], ">", x)) %>% select_(., label) 
  a2 <- filter_(dt, paste(col_name[i], "<=", x)) %>% select_(., label) 
  nrow(a1) / nrow(dt) * giniIndex(a1, label) + 
    nrow(a2) / nrow(dt) * giniIndex(a2, label)
}

###################################################################
## Divide the data set into two subsets
## input: cutoff, number of the column for criteria and data set
## output: a list of two subsets
###################################################################

divideSet <- function(x, i, dt) {
  a1 <- filter_(dt, paste(col_name[i], "<=", x)) 
  a2 <- filter_(dt, paste(col_name[i], ">", x))
  return(list(a1, a2))
}



## Read in the data

#### classic iris example
dt <- iris
label <- "Species"



#### project's data

## Data cleaning

delete_index <- rep_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- inf_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- na_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
dt <- dt[-c(1,  ncol(dt))] ## Exclude file name and other useless information


### Paramemers setting

label <- "Status"
split_measure <- 15  ## how many cutoffs points are used in optimization
max_depth <- 5
min_instance <- 1
info_gain <- 0.001


### stop criteria 1: all instances have the same label

numLabels <- function(dt) {
  a1 <- select_(dt, label) %>% unique %>% nrow
  (a1 == 1)
}

### stop criteria 2: reach maximum depth
maxDepth <- function(nn, mdep) {
  nn >= mdep
}

### stop criteria 3: too few instances for child node
numEx <- function(dt, num) {
  nrow(dt) < num
}

### stop criteria 4: split will not gain info
badSplit <- function(sdt1, sdt2, dt) {
  giniIndex(dt, label) - 
    nrow(sdt1) / nrow(dt) * giniIndex(sdt1, label) -
      nrow(sdt2) / nrow(dt) * giniIndex(sdt2, label)
}

### create leaf (terminal) node
leafNode <- function(dt) {
  a1 <- count_(dt, label)
  z1 <- which.max(a1[[2]]) 
  label <- a1[[1]][z1] %>% as.character
  return(label)
}



buildTree <- function(dt, label, min_instance = 1,
                      max_depth = 10, info_gain = 0.1,
                      n_now = 1) {

  ### Step 0. check early-stopping criteria
  col_name <<- colnames(dt)
  col_name <<- col_name[-which(col_name == label)]
  cat("depth is:", n_now, "\n")
  
  if (numLabels(dt) |
     maxDepth(n_now, max_depth) |
     numEx(dt, min_instance)) {
    cat("E-stop", "\n")
    return(leafNode(dt))
    
  } else {
    
    ### Step 1.
    ### Compute the best split, 
    ### use optim for time-saving 
    
    splitres <- matrix(0, nrow = length(col_name), ncol = split_measure)
    out <- NULL
    
    for (i in seq_along(col_name)) { 
      
      all_cut <- arrange_(dt, col_name[i]) %>% select_(., col_name[i]) %>% unique  ## get the value of i-th column
      cutoffs <- (all_cut[-1,] + all_cut[-length(all_cut),]) / 2
      
      num_split <- ifelse(split_measure < length(cutoffs), split_measure, length(cutoffs))
      
      for (j in seq_len(num_split)) {
        res <- optim(cutoffs[round(length(cutoffs) / j)], impuFun, i = i, dt = dt, method = "BFGS")
        out <- rbind(out, c(res[[1]], res[[2]], i, j))
        #cat(res[[1]], res[[2]], i, j, "\n")
      }
    }
    
    best_split <- which.min(out[, 2])[1]
    best_cut <- out[best_split, 1]
    best_col <- out[best_split, 3]
    
    ### Step 2.
    ### Split the data
    
    divide <- divideSet(best_cut, best_col, dt)
    
    ### Step 3.
    ### check the subset has enough information gain
    cat(badSplit(divide[[1]], divide[[2]], dt),"\n")
    
    if (badSplit(divide[[1]], divide[[2]], dt) < info_gain) {
      cat("Bad Split", "\n")
      return(leafNode(dt))
    }
    
    ### Step final.
    ### return subset
    
    # res <- list(column = best_col, cutoff = best_cut,
    #             L_tree = divide[[1]], R_tree = divide[[2]])
    
    ### Step final + 2
    ### grow subtrees
    
    v1 <- buildTree(res$L_tree, label, min_instance = 1, max_depth = 10,
                     n_now = n_now + 1)
    v2 <- buildTree(res$R_tree, label, min_instance = 1, max_depth = 10,
                     n_now = n_now + 1)
    
    out <- list(column = col_name[best_col],
                cutoff = best_cut,
                depth = n_now,
                left_tree = v1,
                right_tree = v2)
    
    return(out)
  }
}


## Iris example, training

x2 <- buildTree(iris, "Species", min_instance = 1, max_depth = 10, info_gain = 0.001)

#buildTree(dt, label, min_instance = 1, max_depth = 10, info_gain = 0.001)


## Iris trained model display

###################################################################
## Display trained model
## input: a trained model
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
      nametree(X[[i]], paste0(prefix, " "), paste0(prefix_2, "  "))  
    }
  }
}

print_tree(x2)


## Iris used training data to prediciton 

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

for (i in 1:nrow(iris)) {
  cat(prediction(iris[i,], x2), (iris[i, ]$Species) %>% as.character, "\n")
}


