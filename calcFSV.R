findFSV <- function(class_result,train_ftrs,train_cls,classes){

    n_classes <- length(class_result)
    n_ftrs <- ncol(train_ftrs)
    FSV <- vector("list", n_ftrs)
    idx_classes <- which(classes$Class %in% class_result)

    # for each feature we calculate the total standard deviation of the union of
    # all concerned classes and the standard deviation of each class
    for (i in 1:n_ftrs){
        # we select the indexes that correspond to the considered hypotheses
        idx_total_classes <- train_cls$Class %in% classes$Class[idx_classes]
        total_classes_ftrs <- train_ftrs[idx_total_classes,i]
        sd_total <- sd(total_classes_ftrs) #total standard deviation
        product <- 1
        for (j in 1:n_classes){
            idx <- idx_classes[j]
            idx_class <- train_cls$Class %in% classes$Class[idx]
            class_ftrs <- train_ftrs[idx_class,i]
            sd_class <- sd(class_ftrs)
            product <- product * sd_class
        }
        FSV[[i]] <- product/sd_total

    }
    findFSV <- FSV


}