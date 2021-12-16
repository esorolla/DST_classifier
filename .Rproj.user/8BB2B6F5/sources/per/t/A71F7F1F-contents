findDistance <- function(train_ftr,train_cls,ftr,test,class_result,classes){
    n_classes <- length(class_result)
    pred_classes <- unique(train_cls[,1])
    idx_classes <- which(pred_classes %in% class_result)

    distance <- vector("list", n_classes)
    a_test <- test[,ftr]

    for (j in 1:n_classes){
        idx <- idx_classes[j]
        idx_class <- train_cls$Class %in% pred_classes[idx]
        class_ftr <- train_ftr[idx_class,ftr]

        a_mean <- mean(class_ftr)
        
        distance[[j]] <- abs(a_mean-a_test)
    }
    findDistance <- distance

}