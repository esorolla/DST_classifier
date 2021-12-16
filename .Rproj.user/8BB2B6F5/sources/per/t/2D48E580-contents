# This function calculates a BOE as the weighted averages of all the BOEs
# where the weight is given by "CRDmn"
weightedAverage <- function(bcas){

    n_features <- length(bcas)
    CRDmn <- modifiedWeight(bcas) # calculation validation checked

    # we firstly compile the hypotheses from the BOEs
    hypotheses <- character(0)
    counter <- 0
    for (k in 1:n_features){
        bcak <- bcas[[k]]
        ni <- nrow(bcak[["tt"]])
        for (i in 1:ni){
            rowi <- rownames(bcak[["tt"]])[i]
            test <- rowi %in% hypotheses
            if(!test){
                counter <- counter + 1
                hypotheses[[counter]] <- rowi
            }
        }
    }
    final_counter <- counter

    # We calculate the weighted average for each hypothesis by considering the
    # different BOEs. To do so we multiply the masses of the i-th BOE "bcas[[i]]"
    # by "CRDmn[[i]]" and sum all the masses.
    # We need to build a new BOE with all the hypotheses even if the mass
    # associated is zero.
    new_bcas <- rebuildBcas(bcas,hypotheses,CRDmn)
    new_bcas
}


# This function rebuilds every BOE with all the hypotheses including those whose
# mass is null.
rebuildBcas <- function(bcas,hypotheses,CRDmn){

    n_features <- length(bcas)
    classes <- bcas[[1]][["infovaluenames"]][["x"]]
    n_classes <- length(classes)

    hyp <- character(0)
    # we need to add the new hypotheses to the already existing ones
    for(k in 1:n_features){
        bcak <- bcas[[k]]
        hyp_k <- rownames(bcak[["tt"]]) #existing hypotheses
        n_hyp_k <- length(hyp_k)
        hyp_names <- hyp_k
        oldMass <- bcak[["spec"]][,2] #existing masses
        for(i in 1:length(hypotheses)){
            if(hypotheses[[i]]%in%hyp_k){
                next
            }else{
                counter <- n_hyp_k + 1
                hyp_names <- c(hyp_names,hypotheses[[i]])
            }
        }
    }

    # we build the new bca Matrix will all the hypotheses
    bcaNewMatrix <- setNewMatrix(hyp_names,classes)

    # we build the new mass matrix setting to zero the new hypotheses
    newMass <- setNewMass(bcaNewMatrix,bcas,CRDmn)
    rebuildBcas <- bca(bcaNewMatrix,newMass,classes)
    rebuildBcas
}

# This function builds the matrix of the hypothesis
setNewMatrix <- function(hyp_names,classes){

    n_hyp_names <- length(hyp_names)
    n_classes <- length(classes)

    setNewMatrix <- matrix(rep(0,n_hyp_names*n_classes),nrow=n_hyp_names,
                           ncol=n_classes)

    # for each hypothesis we create the row of the bca matrix
    for (i in 1:n_hyp_names){
        hyp_split <- unlist(strsplit(hyp_names[[i]], split = " + ", fixed = TRUE))
        l_hyp <- length(hyp_split)

        if(l_hyp == 1 && hyp_split == "frame"){
            setNewMatrix[i,] <- rep(1,n_classes)
        }else{
            rowi <- c(as.numeric(classes%in%hyp_split))
            setNewMatrix[i,] <- rowi
        }
    }
    rownames(setNewMatrix) <- hyp_names

    # we swap the last row with the one named "frame"
    idx <- which(rownames(setNewMatrix) == "frame")

    setNewMatrix[idx,] <- setNewMatrix[n_hyp_names,]
    rownames(setNewMatrix)[idx] <- rownames(setNewMatrix)[n_hyp_names]
    setNewMatrix[n_hyp_names,] <- rep(1,n_classes)
    rownames(setNewMatrix)[n_hyp_names] <- "frame"

    setNewMatrix
}


setNewMass <- function(bcaNewMatrix,bcas,CRDmn){

    n_hyp_names <- length(bcaNewMatrix[,1])
    n_features <- length(bcas)
    setNewMass <- vector("list", n_hyp_names)

    for (i in 1:n_hyp_names){
        hyp <- rownames(bcaNewMatrix)[i]
        prod <- 0
        for (k in 1:n_features){
            bcak_names <- rownames(bcas[[k]][["tt"]])
            if(hyp %in% bcak_names){ #"hyp" belongs to the original BOE
                idx <- which(bcak_names == hyp)
                aux <- CRDmn[[k]]*bcas[[k]][["spec"]][idx,2]
            }else{
                aux <- 0
            }
            prod <- prod + aux
        }
        setNewMass[[i]] <- prod
    }
    setNewMass <- unlist(setNewMass)
}