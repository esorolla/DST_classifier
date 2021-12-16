################################################################################
#
# This function calculates the modified weight CRDmni as presented in the
# article:
#
# "Weighted Evidence Combination Rule Based on Evidence Distance and
#  Uncertainty Measure: An Application in Fault Diagnosis"
#  Lei Chen, Ling Diao and Jun Sang, Hindawi Mathematical Problems in
#  Engineering, vol. 2018, 2018.
################################################################################
modifiedWeight <- function(bcas){

    n_features <- length(bcas)
    SUP <- supportDegree(bcas) #calculation validation checked

    CRDmi <- vector("list", n_features)
    for (i in 1:n_features){
    # for (i in 1:1){
        complete_bcas <- fillBcas(bcas[[i]])
        supportF <- belplau(complete_bcas)
        U <- sum(supportF[,2] - supportF[,1])

        # calculation validation of CRDi is checked
        CRDi <- SUP[[i]]/sum(unlist(SUP))
        CRDmi[[i]] <- CRDi * exp(U)
    }

    modifiedWeight <- vector("list", n_features)
    for (i in 1:n_features){
        modifiedWeight[[i]] <- CRDmi[[i]]/sum(unlist(CRDmi))
    }
    modifiedWeight
}


# This function calculates the matrix SUP = Sum_j(Sim(mi,mj))
supportDegree <- function(bcas){

    n_features <- length(bcas)

    # we calculate the support degree of each BOE
    supportDegree <- vector("list", n_features)
    for (i in 1:n_features){
    # for (i in 1:1){
        aux <- 0
        # we calculate the sum skipping the diagonal terms
        for (j in 1:n_features){
        # for (j in 3:3){
            if(j!=i){
                DTensor <- calcDTensor(bcas[[i]],bcas[[j]])
                d <- calcDistance(bcas[[i]],bcas[[j]],DTensor)
                SIM <- 1-d
                aux <- aux + SIM
            }else{
                next
            }
        }
        supportDegree[[i]] <- aux
    }

    supportDegree
}

# This function calculates the Jaccard similarity between the different BOEs
calcDTensor <- function(bcai,bcaj){

    classes <- bcai[["infovaluenames"]][["x"]]

    ni <- nrow(bcai[["tt"]])
    nj <- nrow(bcaj[["tt"]])

################################################################################
#   We begin by setting the number of hypotheses existing in the BOEs in order #
#   to allocate the memory of the matrix necessary to calculate the evidence   #
#   distance.                                                                  #
################################################################################

    # we firstly compile the hypotheses from the first BOE
    hypotheses <- character(0)
    counter <- 0
    for (i in 1:ni){
        counter <- counter + 1
        rowi <- rownames(bcai[["tt"]])[i]
        hypotheses[[counter]] <- rowi
    }
    final_counter <- counter

    # we secondly check which hypotheses to add from the second BOE
    for (j in 1:nj){
        rowj <- rownames(bcaj[["tt"]])[j]
        if(rowj%in%hypotheses){
            next
        }else{
            counter <- counter + 1
            hypotheses[[counter]] <- rowj
        }
    }

    n_hyp <- length(hypotheses)

    # we initialize the distance matrix
    calcDTensor <- diag(n_hyp)

    # we compute the similarity distance for the elements that are in bcai
    for (i in 1:n_hyp){
        hyp_i <- hypotheses[[i]]
        S1 <- splitFrame(classes,hyp_i)
        for (j in 1:n_hyp){
            hyp_j <- hypotheses[[j]]
            S2 <- splitFrame(classes,hyp_j)
            if(hyp_i==hyp_j){
                next
            }else{
                calcDTensor[i,j] <- gset_similarity(S1,S2)
            }
        }
    }
    calcDTensor
}


# This function sets the hypotheses in order to calculate the similarity
splitFrame <- function(classes,Si){
    n_classes <- length(classes)
    if(Si == "frame"){
        splitFrame <- gset(as.character(classes[1:n_classes]))
    }else{
        splitFrame <- gset(unlist(strsplit(Si, " + ", fixed = TRUE)))
    }
    splitFrame
}


# This function calculates the evidence distance between different BOEs
calcDistance <- function(bcai,bcaj,DTensor){

    diff <- difference(bcai,bcaj)
    diff <- matrix(diff)

    calcDistance <- sqrt(0.5*t(diff)%*%DTensor%*%diff)
}


# This function calculates the difference of the masses
difference <- function(bcai,bcaj){

    ni <- nrow(bcai[["tt"]])
    nj <- nrow(bcaj[["tt"]])

    difference <- numeric(0)

    # we compute the difference for the elements that are in bcai
    for (i in 1:ni){
        rowi <- rownames(bcai[["tt"]])[i]
        count <- 0
        for (j in 1:nj){
            rowj <- rownames(bcaj[["tt"]])[j]
            # if the row of bcaj exists in bcai, we count it
            if(rowi == rowj){
                count <- count + 1
                idx <- j
            }else{
                next
            }
        }
        #if "count" is non-zero the BOEs share the i-th row
        if(count != 0){
            difference[[i]] <- bcai[["spec"]][i,2] - bcaj[["spec"]][idx,2]
        }else{
            difference[[i]] <- bcai[["spec"]][i,2]
        }
    }

    #absolute counter for the vector "diff"
    counter <- ni
    # we compute now the difference for the elements that are in bcaj
    for (k1 in 1:nj){
        rowj <- rownames(bcaj[["tt"]])[k1]
        count <- 0
        for (k2 in 1:ni){
            rowi <- rownames(bcai[["tt"]])[k2]
            # if the row of bcai exists in bcaj, we count it
            if(rowi == rowj){
                count <- count + 1
            }else{
                next
            }
        }
        #if "count" is non-zero, we have already counted this case
        if(count != 0){
            next
        }else{
            counter <- counter + 1
            difference[[counter]] <- -bcaj[["spec"]][k1,2]
        }
    }
    difference
}

# This function fills the bcas with the singletons that do not appear in the
# original set of hypotheses to calculate the uncertainty for all the cases
fillBcas <- function(bcas){

    fillBcas <- bcas
    hyp <- rownames(bcas[["tt"]])
    classes <- bcas[["infovaluenames"]][["x"]]
    n_classes <- length(classes)

    for (i in 1:n_classes){
        belong_test <- classes[i] %in% hyp
        if(belong_test){
            next
        }else{
            mat <- fillBcas[["tt"]]
            mass <- fillBcas[["spec"]][,2]

            new_vec <- vector("numeric",n_classes)
            new_vec[i] <- 1
            new_mass <- 0

            fillBcas <- bca(rbind(mat,new_vec),c(mass,new_mass),classes,
                            infovarnames = "x", varnb=1)
        }
    }
    fillBcas <- nzdsr(fillBcas)
    fillBcas
}