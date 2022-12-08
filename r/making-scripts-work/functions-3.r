source("functions-2.r")

remove.constants <- function(variables) {
    ss <- rep(NA,ncol(variables))
    for (i in 1:ncol(variables))
        ss[i] <- var(variables[,i])
    browser()                               #### start the code browser
    is.constant <- ss < 2e-16
    if (any(is.constant)) {                 ## where the error was generated
        warning("Omitting variables with zero variance: ", sum(is.constant))
        variables <- variables[,!is.constant,drop=F]
    }
    variables
}
