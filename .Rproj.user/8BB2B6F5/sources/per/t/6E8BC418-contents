################################################################################
#
# This function applies the DST to combine attribute values from a data set so
# that a classifier can be built to predict new outcomes from future data.
# The function calls other functions to accomplish the task.
#
################################################################################
# This version includes the weighted average of the BOEs from the different
# features as depicted in the article:
#
# "Weighted Evidence Combination Rule Based on Evidence Distance and
#  Uncertainty Measure: An Application in Fault Diagnosis"
#  Lei Chen, Ling Diao and Jun Sang, Hindawi Mathematical Problems in
#  Engineering, vol. 2018, 2018.
#
################################################################################

classify <- function(train,validation){

    # we split the features from the classes
    train_features <- train[,1:ncol(train)-1]
    train_classes <- data.frame("Class" = as.character(train[,ncol(train)]))
    validation_features <- validation[,1:ncol(train)-1]
    validation_classes <- data.frame("Class" = 
                                    as.character(validation[,ncol(validation)]))
    # 1) Function to find the min-max attribute values for each class.
    #    The output is a list of dataframes where the columns represent the
    #    classes and each dataframe presents the min-max value for a single
    #    feature.
    classes <- unique(train_classes)
    n_classes <- length(classes[,1])
    rownames(classes) <- NULL
    minmax <- MinMax(train_features,train_classes,classes)

    val_set_size <- dim(validation_features)[1]
    class_finally <- vector("list", val_set_size)

    # we loop over the test instances:
    for (i in 1:val_set_size){
    # for (i in 1:1){
        test <- validation_features[i,]

        # 2) Function to obtain the BOEs from each feature.
        #    The output is a "bca" object defined in the "dst" package.
        bcas <- masses(minmax,test,classes)
        n_features <- length(bcas)

        # 3) Function to calculate the weighted average of the BOEs from the
        #    different features.
        mWAE <- weightedAverage(bcas)


        # 4) Function to combine the weighted average of the BOEs with itself
        #    k-1 times, where k is the number of features (of evidences).
        netBCA <- combine(mWAE,n_features)

        # 5) Function to find the hypothesis with the highest associated mass
        #    function (the author of the article above proposes to pick the
        #    hypothesis with the highest belief function, but this is always
        #    the one corresponding to the whole frame of discernment. Thus, we
        #    propose to pick the hypothesis with the highest mass).
        class_result <- maxMass(classes,netBCA)
        class_result <- unlist(strsplit(class_result, split = "+", fixed = TRUE))
        class_result <- trimws(class_result,"both")
        class_result <- unique(class_result)


        if (length(class_result) == 1){
            class_finally[[i]] <- class_result
        }else{

            # 6) Function to calculate the FSV of each feature for the classes
            #    that belong to the hypothesis stored in "class_result".
            FSV <- findFSV(class_result,train_features,train_classes,classes)
            FSV <- unlist(FSV)

            # 7) Function to select the feature with the smallest FSV
            selected_ftr <- which(FSV == min(FSV))

            # 8) Function to calculate the distance between the selected
            #    attribute of the data item and the mean value for each class.
            dist <- findDistance(train_features,train_classes,selected_ftr,test,
                                 class_result,classes)
            dist <- unlist(dist)

            # 9) Function to find the smallest distance (the class satisfying
            #    this is the searched result).
            class_finally[[i]] <- class_result[which(dist == min(dist))]
            # print(i)
            # print(class_finally[[i]])

        }

    }
    row_names <- paste0("actual ", classes$Class)
    col_names <- paste0("predict ", classes$Class)
    conf_matrix <- matrix(c(rep(0,n_classes*n_classes)),nrow = n_classes,
                          ncol = n_classes,byrow = TRUE,
                          dimnames=list(row_names,col_names))

    for (k1 in 1:n_classes){
        actual_class <- classes[k1,1]
        for (k2 in 1:n_classes){
            pred_class <- classes[k2,1]
            count <- 0
            for (j in 1:val_set_size){
                if (class_finally[[j]] == pred_class 
                    && validation_classes[j,1]==actual_class){
                    count <- count + 1
                }
            }
            conf_matrix[k1,k2] <- count
        }
    }
	# we sort the columns and rows of the matrix so as to be sure that we
	# correctly sum the right elements for each fold
	conf_matrix <- conf_matrix[,sort(colnames(conf_matrix))]
    conf_matrix <- conf_matrix[sort(rownames(conf_matrix)),]
    # we evaluate the score of the correct results
    score <- sum(class_finally == validation_classes[,1])/val_set_size*100
    classify <- list(score,conf_matrix)
}