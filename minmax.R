# This function finds the min-max attribute values for each class so that we
# can sort the classes according to the feature value of the test data item.

MinMax <- function(train_ftrs,train_cls,classes){

    n_classes <- lengths(classes)
    n_ftrs <- ncol(train_ftrs)
    names <- unlist(classes)
    intervals <- vector("list",n_ftrs)

# we find the boundaries of each class throughout the feature values
    for (i in 1:n_ftrs){
        output <- matrix(ncol=n_classes, nrow=2)
        for (j in 1:n_classes){
            output[1,j] <- min(train_ftrs[train_cls == classes[j,1],i])
            output[2,j] <- max(train_ftrs[train_cls == classes[j,1],i])
        }
        output <- data.frame(output)
        colnames(output) <- names
        intervals[[i]] <- output
    }

    MinMax <- intervals
    MinMax
}