combine <- function(mWAE,n){
    comb <- mWAE
    # we combine the weighted average BOE ("mWAE") with itself n-1 times, where
    # n is the number of original BOEs
    for (i in 1:(n-1)){
        # we do not normalize with the conflict constant
        comb <- dsrwon(comb,mWAE)
    }
    comb
}