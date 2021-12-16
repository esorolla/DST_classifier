maxMass <- function(classes,netBCA){
    if(netBCA$con == 1){ #original (maximum mass probability):
        idx_max <- which(netBCA[["spec"]][,2] == max(netBCA[["spec"]][,2]))
        maxMass <- rownames(netBCA[["tt"]])[idx_max]
    }else{ #alternative (maximum Belief function):
        aux <- belplau(nzdsr(netBCA))

        #alternative:
        idx_max <- which(aux[-c(length(aux[,1])),1] == max(aux[-c(length(aux[,1])),1]))
        maxMass <- rownames(aux)[idx_max]
    }

    # we deal with the case when several hypotheses hold the maximum value
    len_maxMass <- length(maxMass)
    if (len_maxMass > 1){ #there are several maxima
        aux <- maxMass[[1]]
        for (i in 2:len_maxMass){
            aux <- paste0(aux, ' + ', maxMass[[i]])
        }
        maxMass <- aux
    }

    #we implement the case when phi (empty set) is the hypothesis with the
    #largest bca. We pick the next largest bca as the correct hypothesis.
    #The empty set is named by its unicode code '\u00f8'
    if (maxMass == '\u00f8'){
        # print("netBCA:")
        # print(netBCA)
        netBCA_noEmpty <- netBCA[["spec"]][-c(idx_max),]
        # we remove the row with the empty hypothesis to find the max. among the
        # rest of hypotheses (the empty hypothesis as prediction is not valid)
        netBCA_noEmpty_tt <- netBCA[["tt"]][-c(idx_max),]
        idx_max_noEmpty <- which(netBCA_noEmpty[,2] == max(netBCA_noEmpty[,2]))
        maxMass <- rownames(netBCA_noEmpty_tt)[idx_max_noEmpty]
    }else if(maxMass == "frame"){
        class_hyp <- classes[1,1]
        for (k in 2:(length(classes[,1]))){
            class_hyp <- paste0(class_hyp,paste0("+",classes[k,1]))
        }
        maxMass <- class_hyp
    }
    maxMass
}