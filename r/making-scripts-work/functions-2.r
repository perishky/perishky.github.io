source("functions-1.r")

remove.constants <- function(variables) {
    ss <- rep(NA,ncol(variables))                           ## `ss` will hold the variances
                                                            ##         of each variable
    for (i in 1:ncol(variables))                            ## for each variable i
        ss[i] <- var(variables[,i])                         ##   ss[i] = variance of variable i
    is.constant <- ss < 2e-16                               ## is.constant == TRUE for all variables
                                                            ##         with variance < 2x10^-16
    if (any(is.constant)) {                                 ## if any variable has low variance
        warning("Omitting variables with zero variance: ",  ##   issue a warning that they will be removed
                sum(is.constant))                           ## 
        variables <- variables[,!is.constant,drop=F]        ##   remove those variables
    }
    variables
}

reduce.dimensions<-function(variables, effects, num) {
    variables <- remove.constants(variables) ####### added this line
    variables <- scale(variables)
    variables <- remove.effects(variables, effects)
    reduced.variables <- compute.pcs(variables, num)
    reduced.variables
}
