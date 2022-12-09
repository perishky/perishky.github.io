## Captures variance with fewer variables
## Parameters:
## - variables: matrix of variables (columns=variables, rows=observations)
## - effects: matrix of effects (columns=effects, rows=observations)
## - num: number of desired output variables 
## Output:
## A matrix of `num` variables that capture variation in `variables`
## but none of the variation in `effects`.
## 
reduce.dimensions <- function(variables, effects, num) {  
    variables <- scale(variables)                         ## standardize variables (mean=0, sd=1)
    variables <- remove.effects(variables, effects)       ## remove effects from variables
    reduced.variables <- compute.pcs(variables, num)      ## reduce variables to `num` variables
    reduced.variables                                     
}

## Removes effects from variables
## Parameters:
## - variables: matrix of variables (columns=variables, rows=observations)
## - effects: matrix of effects (columns=effects, rows=observations)
## Output:
## The residuals of fitting the model "variables ~ effects".
## 
remove.effects <- function(variables, effects) {
    fit <- lm.fit(x=effects, y=variables)
    residuals(fit)
}

## Obtain the top principal components
## Parameters:
## - variables: matrix of variables (columns=variables, rows=observations)
## - num: number of desired output variables 
## Output:
## The top `num` principal components of `variables`.
## 
compute.pcs <- function(variables, num) {
    fit <- prcomp(variables)                              ## perform PCA
    pcs <- fit$x                                          ## extract PCs
    pcs <- pcs[,1:num,drop=F]                             ## select top `num` PCs
    pcs
}

## Remove variables (columns) that do not vary.
## - variables: matrix of variables (columns=variables, rows=observations)
## Output:
## The input matrix without columns that did not vary.
## 
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
