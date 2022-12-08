source("functions-2.r")

remove.constants <- function(variables) {
    ss <- rep(NA,ncol(variables))
    for (i in 1:ncol(variables))
        ss[i] <- var(variables[,i], na.rm=T) ###### added na.rm=T
    is.constant <- ss < 2e-16
    if (any(is.constant)) {
        warning("Omitting variables with zero variance: ", sum(is.constant))
        variables <- variables[,!is.constant,drop=F]
    }
    variables
}
