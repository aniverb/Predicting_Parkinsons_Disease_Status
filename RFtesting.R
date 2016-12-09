rm(list=ls())

library(compiler)
compilePKGS(TRUE)
setCompilerOptions(suppressAll = TRUE, optimize = 3)
enableJIT(3)
library(lazyeval)
library(dplyr)
library(magrittr)
library(foreach)
library(doParallel)
library(party)
library(FSelector)
library(caret)

#cl<-makeCluster(4)
# registerDoParallel(cl)
# clusterCall(cl, function() {library(lazyeval); library(dplyr); library(magrittr); library(foreach); library(doParallel); library(party)})

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

divideSet <- function(cut, i, dt, col_name) {
  a1 <- dt[(dt[[col_name[i]]] <=  cut),]
  a2 <- dt[(dt[[col_name[i]]] > cut),]
  # a1 <- filter_(dt, paste(col_name[i], "<=", cut))
  # a2 <- filter_(dt, paste(col_name[i], ">", cut))
  return(list(a1, a2))
}
# a1 <- subdt[(subdt[, 1] > x), 2] 
# a2 <- subdt[(subdt[, 1] <= x), 2] 


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

###################################################################
## Combine impuFun and giniindex
## input: sub data set, cutoff
## output: gini impurity
###################################################################

giniImpu <- function(x, subdt, tt = nrow(subdt)) {
  a1 <- subdt[(subdt[, 1] > x), 2] 
  a2 <- subdt[(subdt[, 1] <= x), 2] 
  len_a1 <- length(a1)
  len_a2 <- length(a2)
  frac_a1 <- table(a1) / len_a1
  frac_a2 <- table(a2) / len_a2
  ((len_a1 * (1 - sum(frac_a1 ^ 2)) ) + 
    (len_a2 * (1 - sum(frac_a2 ^ 2)) )) / tt
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
  col_name <- c(col_name, label)
  
  st <- select_(dt, .dots = col_name)
  
  cat("depth is:", n_now, "\n")

  if (numLabels(dt, label) | maxDepth(n_now, max_depth) |
      numEx(dt, min_instance)) {
    
    cat("Early stopping", "\n")
    return(leafNode(dt, label))
    
  } else {
    
    ### Step 1.
    ### Compute the best split, 
    
    infoG <- information.gain(as.formula(paste(label, "~.")), st)
    best_col <- which.max(infoG[, 1])
    
    all_cut <- arrange_(st, col_name[best_col]) %>% 
      select_(., col_name[best_col]) %>%
      unique  ## get the value of i-th column
    
    cutoffs <- (all_cut[-1,] + all_cut[-nrow(all_cut),]) / 2
    
    subdt <- st[, c(best_col, ncol(st))]

    ## Method. 1
    
    #a <- Sys.time()
    out <- sapply(seq_along(cutoffs), function(zz) {
      giniImpu(cutoffs[zz], subdt)
    }) 
    best_cut <- cutoffs[which.min(out)]
    #b <- Sys.time()
    
    ## Method. 2
    # out <- foreach(j=sequ, .combine = "rbind",
    #                .export=c('giniImpu')) %dopar% {
    #                  optim(cutoffs[j], giniImpu, subdt = subdt)}
    # best_cut <- out[which.min(out[,2]),1] %>% unlist

    ## Method. 3
    # out <- foreach(j=seq_along(cutoffs),
    #                .combine = "c",
    #                .export=c('giniImpu')) %dopar% {
    #                  giniImpu(cutoffs[j], subdt)}
    # best_cut <- cutoffs[which.min(out)]
    
    ### Step 2.
    ### Split the data
    
    divide <- divideSet(best_cut, best_col, dt, col_name)
    
    ### Step 3.
    ### check the subset has enough information gain
    
    gini_gain <- badSplit(divide[[1]], divide[[2]], dt, label)

    if (gini_gain < info_gain) {
      cat("Info gain is:", gini_gain, "<", info_gain, "\n")
      cat("Bad Split", "\n")
      return(leafNode(dt, label))
    }
    
    # if (badSplit(divide[[1]], divide[[2]], dt, label) < info_gain) {
    #   cat("Info gain is:", badSplit(divide[[1]], divide[[2]], dt, label),
    #       "<", info_gain, "\n")
    #   cat("Bad Split", "\n")
    #   return(leafNode(dt, label))
    # }
    
    res <- list(column = best_col, cutoff = best_cut,
                L_tree = divide[[1]], R_tree = divide[[2]])
    
    ### Step final 
    ### grow subtrees
    
    v1 <- buildTree(res$L_tree, label, min_instance = min_instance,
                    max_depth = max_depth, n_now = n_now + 1,
                    info_gain = info_gain, split_measure = split_measure,
                    numOfFeatures = numOfFeatures) 
    v2 <- buildTree(res$R_tree, label, min_instance = min_instance,
                    max_depth = max_depth, n_now = n_now + 1,
                    info_gain = info_gain, split_measure = split_measure,
                    numOfFeatures = numOfFeatures) 
    
    tree <- list(column = col_name[best_col],
                 cutoff = best_cut,
                 depth = n_now,
                 left_tree = v1,
                 right_tree = v2,
                 info_gain = gini_gain / nrow(dt))
    
    return(tree)
  }
}

###################################################################
## Make prediction
## input: a new instance, a trained model
## output: a prediction
###################################################################

prediction <- function(instance, model) {
  if (is.character(model)) {
    return(model %>% as.character)
  } 
  
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

importance <- function(fit, out_name = NULL, out_weight = NULL) {
  if (fit$column %in% out_name) {
    index <- which(fit$column == out_name) 
    out_weight[index] <- fit$info_gain + out_weight[index]
  } else {
    out_name <- c(out_name, fit$column) 
    out_weight <- c(out_weight, fit$info_gain)
  }
  
  if (is.list(fit$left_tree)) {
    v1 <- importance(fit$left_tree, out_name = out_name,
                     out_weight = out_weight)
  } else {
    v1 <- NULL
  }
  
  if (is.list(fit$right_tree)) {
    v2 <- importance(fit$right_tree, out_name = out_name,
                    out_weight = out_weight)
  } else {
    v2 <- NULL
  }
  
  out_name <- c(out_name, v1, v2)
  out_weight <- c(out_weight, v1, v2)
  
  return(list(out_name, out_weight))
}
#importance(x2)

importance <- function(fit, out = NULL) {
  
  out <- rbind(out, c(fit$column, fit$info_gain))

  if (is.list(fit$left_tree)) {
    v1 <- importance(fit$left_tree)
  } else {
    v1 <- NULL
  }
  
  if (is.list(fit$right_tree)) {
    v2 <- importance(fit$right_tree)
  } else {
    v2 <- NULL
  }
  
  out <- rbind(out, v1, v2)
  
  return(out)
}

# x1 <- importance(x2)
# x1[ x1[,1] == (x1[,1] %>% unique),2] %>% as.numeric() %>% sum


## Iris trained model display

###################################################################
## Display trained tree/model
## input: a trained tree/model
## output: tree display
###################################################################

print_tree <- function(X, prefix = "", prefix_2 = " ") {
  
  #cat("* indicates a leaf node", "\n")
  #cat("", "\n")
  if (is.character(X)) {
    cat("root node", "\n")
    return(cat(as.character(X), "*", "\n"))
  }
  
  for (i in c(1, 4, 5)) {
    if (i == 1) {
      cat(prefix, paste(X[[3]], ")", sep = ""), X[[i]],
          "<=", round(X[[2]], 3), "\n", sep = " ")
    } else if (i == 4) {
      if (!is.list(X[[i]])) {
        cat(prefix_2, paste(X[[3]], ")", sep = ""),
            "True :", X[[i]], "*", "\n", sep = " ")
      } else {
        cat(prefix_2, paste(X[[3]], ")", sep = ""), "True :", X[[1]],
            "<=", round(X[[2]], 3), "\n", sep = " ")
        print_tree(X[[i]], paste0(prefix, "  "), paste0(prefix_2, "  "))
        
      }
    } else if (i == 5) {
      if (!is.list(X[[i]])) {
        cat(prefix_2, paste(X[[3]], ")", sep = ""),
            "False:", X[[i]], "*", "\n", sep = " ")
      } else {
        cat(prefix_2, paste(X[[3]], ")", sep = ""), "False:", X[[1]],
            ">", round(X[[2]], 3), "\n", sep = " ")
        print_tree(X[[i]], paste0(prefix, "  "), paste0(prefix_2, "  "))
        
      }
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


# buildForest=function(numOfTrees, dt, label, min_instance = 1, max_depth = 10, info_gain = 0.1, n_now = 1, split_measure = 15, numOfFeatures=NA){
#   forest = foreach (i = 1:numOfTrees) %dopar% {
#     cat(i, "\n")
#     dt=bootstrap(dt)
#     buildTree(dt, label, min_instance, max_depth, info_gain, n_now, split_measure, numOfFeatures)
#   }
#   return(forest)
# }

buildForest=function(numOfTrees, dt, label, min_instance = 1, max_depth = 10, info_gain = 0.1, n_now = 1, split_measure = 15, numOfFeatures=NA){
  forest=list()
  for (i in 1:numOfTrees){
    cat(i, "\n")
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


# forestPredict=function(dt, forest){
#   numtrees=length(forest)
#   n=nrow(dt)
#   predictionsMat=matrix(nrow=n, ncol=numtrees)
#   id=0
#   foreach(tree=forest, .export=c('prediction'))%dopar%{
#     predictions=c()
#     foreach(i= 1:nrow(dt), .export=c('prediction'))%dopar% {
#       predictions[i]=prediction(dt[i,], tree)
#     }
#     id=id+1
#     predictionsMat[,id]= predictions
#   }
#   maxVotes=apply(predictionsMat, 1, countVote)
#   return(maxVotes)
# }

forestPredict=function(dt, forest){
  numtrees=length(forest)
  n=nrow(dt)
  predictionsMat=matrix(nrow=n, ncol=numtrees)
  id=1
  for (tree in forest){
    predictions=c()
    cat("tree #:", id, "\n") #3 err
    for (i in 1:n) {
      #cat("obs #:", i, "\n") #4019 err
      predictions[i]=prediction(dt[i,], tree) 
    }
    predictionsMat[,id]= predictions
    id=id+1
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

