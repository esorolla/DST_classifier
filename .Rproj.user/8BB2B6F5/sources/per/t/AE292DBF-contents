################################################################################
#                                                                              #
# This script calls the classifier implemented by using the DST                #
# (Dempster-Shafer Theory) based on the article:                               #
#                                                                              #
# "Data classification using the Dempster-Shafer method" by Qi Chen,           #
# A. Whitbrook, U. Aickelin and C. Roadknight, Journal of Experimental &       #
# Theoretical Artificial Intelligence, vol. 26, No. 4, 493-517, 2014.          #
#                                                                              #
################################################################################
# we load all the required functions
source("minmax.R")
source("masses.R")
source("combine.R")
source("findMaxMass.R")
source("calcFSV.R")
source("distance.R")
source("DST_classifier.R")
source("classify.R")
source("weightedAverage.R")
source("modifiedWeight.R")

library(caret)
library(dst)
library(tidyverse)
library(sets)
library(outliers)

# the user can load the data from this script:
# source("./read_data.R")

###############################################################################
#                                                                             #
# We load the iris data                                                       #
#                                                                             #
data("iris")
myData <- iris
colnames(myData)[which(colnames(myData)=="Species")] <- "Class"
#                                                                             #
###############################################################################
df <- myData

# Create n_folds equally-sized folds
# (can be modified by the user)
n_folds <- length(myData$Class)/10
outcome <- DST_classify(df,n_folds,ratio)
    