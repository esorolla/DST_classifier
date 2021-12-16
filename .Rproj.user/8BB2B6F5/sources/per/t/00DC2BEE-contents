# This function assigns the mass probability to the hypothesis found previously.
# According to the article, 0.9 is assigned to the hypothesis when one or two
# classes are considered and 0.1 to the frame. When the three classes are the
# hypothesis, the mass probability assigned to the frame is the unity.
masses <- function(minmax,test,classes){
    library(dplyr)

    n_ftrs <- length(minmax)
    n_classes <- lengths(classes)
    n_hyp <- 2^(n_classes)
    name_classes <- classes[,1]
    hyp_names <- combNames(minmax,n_hyp,n_classes) #power set of hypotheses
    masses <- vector("list",n_ftrs) # data frame with the masses corresponding
                                    # to each element of the power set
    extreme_vals <- findExtremes(minmax,test)

    # we implement the case when the test feature value is BELOW the smallest
    # one or ABOVE the largest one within the training data set
    store <- vector("list",n_classes) #vector to store the classes that the
                                      #data item may belong to for each feature.
    for (i in 1:n_ftrs){
        # print(extreme_vals[ncol(minmax[[i]]),i])
        if (test[,i] < extreme_vals[1,i]){#we re-define the class 1 with the
                                          #test data item
            store[1] <- 1
            store[2:n_classes] <- 0
        }else if(test[,i] > extreme_vals[3,i]){# we re-define the class
                                               # "n_classes" with the test
                                               # data item
            store[1:(n_classes-1)] <- 0
            store[n_classes] <- 1
        }else{
            for (j in 1:n_classes){
                logi_class <- between(test[,i],minmax[[i]][1,j],minmax[[i]][2,j])

                # if the value of the feature of the data item is within the range
                # we set "store" to 1, otherwise, to 0.
                if (logi_class){
                    store[[j]] <- 1
                }else{
                    store[[j]] <- 0
               }
            }
        }
        masses[[i]] <- setHypothesis(store,n_classes,name_classes)
    }
    masses
}


# This function finds the power set from the hypothesis sets by combining all
# the singletons.
combNames <- function(minmax,n_hyp,n_classes){
    names <- colnames(minmax[[1]])
    combNames <- vector("list",n_hyp)
    hyp_counter <- 1 #hypothesis counter

    for (i in 1:(n_classes)){
        combinations <- combn(names[1:n_classes],i)
        n_rows <- nrow(combinations)
        n_cols <- ncol(combinations)

        if (n_rows == 1){
            for (j in hyp_counter:(hyp_counter + n_cols-1)){
                combNames[[j]] <- combinations[,j]
            }
        }

        if (n_rows > 1){
            for (j in hyp_counter:(hyp_counter + n_cols-1)){
                k <- j-hyp_counter+1
                combNames[[j]] <- concatenate(combinations[,k])
            }
        }
        hyp_counter <- hyp_counter+n_cols # we update the counter
    }
    combNames[[hyp_counter]] <- "frame"
    combNames

}

# This function adds "+" between classes when more than one is considered in
# the hypothesis set.
concatenate <- function(combinations){
    n_rows <- length(combinations)
    concatenate <- combinations[1]

    for (i in 2:n_rows){
        concatenate <- paste0(concatenate,"+",combinations[i])
    }
    concatenate
}


# This function assigns the masses to the hypothesis fields of the dataframe of
# the corresponding feature
setHypothesis <- function(store,n_classes,name_classes){
    alpha <- 0.9  # parameter can be modified by the user

    filling <- rep(1,n_classes)

    if(sum(unlist(store)) == n_classes){
        setHypothesis<- matrix(c(filling),ncol=n_classes, nrow=1,byrow=TRUE)
        m <- c(1)
    }else{
        setHypothesis<- matrix(unlist(c(store,filling)),ncol=n_classes, nrow = 2,
                               byrow=TRUE)
        m <- c(alpha,1-alpha)
    }

    setHypothesis <- bca(setHypothesis,m,name_classes,
                         infovarnames = "x", varnb=1)
    setHypothesis
}


# This function finds the extreme values for each feature and the class whose
# bounds correspond to these extremes
findExtremes <- function(minmax,test){
    n_ftrs <- length(minmax)

    # this matrix stores row-wise the minimima and the maxima with the
    # associated class for each feature (column-wise)
    findExtremes <- data.frame(matrix(ncol = ncol(test),nrow = 4))

    for (i in 1:n_ftrs){
        min_ftr <- min(minmax[[i]])
        max_ftr <- max(minmax[[i]])
        min_cls <- colnames(minmax[[i]])[which(minmax[[i]] == min(minmax[[i]]),
                                                arr.ind=TRUE)][2]
        max_cls <- colnames(minmax[[i]])[which(minmax[[i]] == max(minmax[[i]]),
                                                arr.ind=TRUE)][2]

        findExtremes[1,i] <- as.numeric(min_ftr)
        findExtremes[2,i] <- min_cls
        findExtremes[3,i] <- as.numeric(max_ftr)
        findExtremes[4,i] <- max_cls
    }
    findExtremes
}

# This function calculates the indexes of the classes bounding the parameter
# value of the test date lying outside any of the classes of the data set.
empty_fill <- function(test,minmax_df,classes){
    n_classes <- lengths(classes)

    distance_min <- vector("list",n_classes)
    distance_max <- vector("list",n_classes)
    for (j in 1:n_classes){
        minimum <- minmax_df[1,j]
        maximum <- minmax_df[2,j]
        distance_min[[j]] <- abs(test - minimum)
        distance_max[[j]] <- abs(test - maximum)
    }

    min_cls <- colnames(minmax_df)[which(unlist(distance_min) == min(unlist(distance_min)),
                                arr.ind=TRUE)]
    max_cls <- colnames(minmax_df)[which(unlist(distance_max) == min(unlist(distance_max)),
                                         arr.ind=TRUE)]

    idx_min <- which(unlist(distance_min) == min(unlist(distance_min)),
                     arr.ind=TRUE)
    idx_max <- which(unlist(distance_max) == min(unlist(distance_max)),
                     arr.ind=TRUE)

    empty_fill <- list(idx_min,idx_max)
}