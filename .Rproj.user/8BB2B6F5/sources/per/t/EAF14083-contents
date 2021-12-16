################################################################################
#                                                                              #
# This function implements a classifier using the DST (Dempster-Shafer Theory) #
# based on the article:                                                        #
#                                                                              #
# "Data classification using the Dempster-Shafer method" by Qi Chen,           #
# A. Whitbrook, U. Aickelin and C. Roadknight, Journal of Experimental &       #
# Theoretical Artificial Intelligence, vol. 26, No. 4, 493-517, 2014.          #
#                                                                              #
#                                                                              #
# In order to apply the DST we must define the analogies between this theory   #
# and the multi-class classifier problem.                                      #
#                                                                              #
# a) The features of data correspond to the experts within the DST framework.  #
# b) The classes of data correspond to the hypotheses from the frame of        #
#    discernment or Universal Set.                                             #
#                                                                              #
# The steps of the algorithm are the following:                                #
#                                                                              #
# 1) For each feature we find the max. and the min. values associated to each  #
#    class. The overlapping of the features' values for different classes      #
#    corresponds to the parameter uncertainty within the DST framework.        #
# 2) We use the DRC (Dempster Rule of Combination) across all the features to  #
#    produce overall mass values for each hypothesis, and the hypothesis with  #
#    the highest belief value will be used to classify the data item.          #
# 3a) If the hypothesis does not represent a single class, we calculate the    #
#     standard deviation for each feature and each concerned class among the   #
#     ones within the estimated hypothesis and for the union of all classes.   #
# 3b) The feature with the smallest FSV (see article), a, is taken and the     #
#     absolute difference, d, between the data item's a value and the mean a   #
#     value is calculated for each class.                                      #
# 3c) The data item is classified as belonging to the class z with the smallest#
#     d value.                                                                 #
################################################################################
#                                                                              #
# In this version we use the Leave-One-Out-Validation (LOOV)).                 #
#                                                                              #
# We also include the weighting average of the BOEs based on the evidence      #
# distance and the application of the combination of the BOE on itself         #
# k-1 times.                                                                   #
#                                                                              #
################################################################################
DST_classify <- function(myData,n_folds,ratio){

    accuracy <- vector("list", n_folds)
    empty <-matrix(ncol = 2, nrow = 1)
    df <- data.frame(empty)
    plot_names <- c("fold", "accuracy")
    colnames(df) <- plot_names
    sum_acc <- 0
    set.seed(10001)

    # We create the folds from the train dataset to split into training and
    # validation
    folds <- createFolds(myData$Class, k = n_folds, list = TRUE,
                         returnTrain = TRUE)
    names(folds)[1] <- "train"

    # we loop over the folds:
    #Perform n_folds LOOV (Leave-One-Out-Validation)
    for(i in 1:n_folds){
        trainData <- myData[folds[[i]],]
        valData <- myData[-c(folds[[i]]),]

        rownames(valData) <- NULL
        rownames(trainData) <- NULL

################################################################################

        #we calculate the accuracy of the predictions
        outcome <- classify(trainData,valData)
        accuracy[[i]] <- outcome[[1]]

        if (i==1){
            conf_matrix <- outcome[[2]]
        }else{
            conf_matrix <- conf_matrix+outcome[[2]]
        }

        sum_acc <- sum_acc+accuracy[[i]]
        mean <- round(sum_acc/i,1)
        aux1 <- i
        aux2 <- accuracy[[i]]
        aux <- c(aux1,aux2)
        df <- rbind(aux,df)

        gplot <- ggplot(df)
        gplot <- gplot+geom_line(aes(x = fold, y = accuracy))+
                    geom_point(aes(x = fold, y = accuracy))+
                    scale_x_continuous(name = "# fold",
                           breaks=seq(0,n_folds,10),limits=c(0.5,n_folds+0.5))+
                    scale_y_continuous(name = "Accuracy (%)",
                           breaks=seq(10,100,10), limits = c(0,100))+
        geom_text(aes(x = n_folds/2,y=50,label=paste0("Mean accuracy:", mean)),
                  color= alpha("red",1))+
        geom_text(aes(x = n_folds/2,y=60,label="Leave-one-out-validation (v.5)"),
                  color = alpha("red",1))+
        theme(legend.position = "none")
        # geom_text(aes(x = n_folds/2,y=40,label=paste0("train/test ratio:", ratio)))
        print(gplot)
    }

    cat("confusion_matrix: \n")
    print(conf_matrix)
    cat("\n\nPositive Predicted Value from the different classes:\n")

    for (k in 1:nrow(conf_matrix)){
        total <- sum(conf_matrix[,k])
        PPV <- conf_matrix[k,k]/total
        class <- trimws(str_extract(rownames(conf_matrix)[k],"\\s.*"),"both")
        output <- paste0("PPV of class ", class,": ",round(100*PPV,digits=1),"%")
        print(output)
    }

    file_save_name <- "Iris_acc_vs_folds_alpha_0,9.png"
    ggsave(file_save_name, width = 20, height = 10, units = "cm")
    DST_classify <- accuracy
}