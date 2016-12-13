setwd("C:/Users/Tri/Documents/DSSCode")
rm(list = ls())
source("dssFunctionLibrary.R")
source("dssDataSetSpecificHacks.R")
source("dssPerformanceEvaluation.R")
source("mimicUsefulFunction.R")
require(gridExtra)
require(ROCR)
require(ISLR)
require(caret)
require(magrittr)

test_0 <- read.csv("test0_roch_all_patients.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE,
                   comment.char = "", na.strings = c("NA", "#DIV/0!", ""))
test_1 <- read.csv("test1_roch_all_patients.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, 
                   comment.char = "", na.strings = c("NA", "#DIV/0!", ""))
test_2 <- read.csv("test2_roch_all_patients.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, 
                   comment.char = "", na.strings = c("NA", "#DIV/0!", ""))
test_3 <- read.csv("test3_roch_all_patients.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE,
                   comment.char = "", na.strings = c("NA", "#DIV/0!", ""))
test_4 <- read.csv("test4_roch_all_patients.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE,
                   comment.char = "", na.strings = c("NA", "#DIV/0!", ""))
UPDRS <- read.csv("UPDRS.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE,
                  comment.char = "", na.strings = c("NA", "#DIV/0!", ""))

bigdt <- combineDatasets()

dt <- bigdt

delete_index <- rep_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- nonpair_find(dt)
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- inf_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- na_find(dt[3:(ncol(dt)-1)])
if (length(delete_index) > 0) dt <- dt[-delete_index,]
delete_index <- outlier_find(dt[3:(ncol(dt)-1)], 3)
if (length(delete_index) > 0) dt <- dt[-delete_index,]

dt$Filename <- as.character(dt$Filename)
unique_patients <- getUnique.ids(dt)
num_patients <- unique(unique_patients)
dt$id <- unique_patients
valIndexes <- getValidateIndexes(dt)
valSet <- dt[valIndexes,]
other <- dt[-valIndexes,]
reg.params <- sweep.lambdas(valSet,3,test_number)
1-lambda.validate(other,reg.params[1],reg.params[2],reg.params[3],3,test_number,F,F) #evaluate accuracy on test
