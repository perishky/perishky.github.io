########################
## Example: dimension reduction

source("functions.r")

########################
## Reducing dimensions

n <- 50                 ## 50 observations/samples
n.vars <- 20            ## 20 variables
n.effs <- 5             ## 5 effects to remove
variables <- matrix(rnorm(n.vars*n),nrow=n)
effects <- matrix(rnorm(n.effs*n), nrow=n)

new.vars <- reduce.dimensions(variables,effects,5)

quantile(cor(variables, effects))
quantile(cor(new.vars, effects))
quantile(cor(new.vars, variables))

########################
## Encountering an error

variables[,4] <- 0

new.vars <- reduce.dimensions(variables,effects,5)

########################
## Identifying the problem using debug()

debug(reduce.dimensions)
new.vars <- reduce.dimensions(variables,effects,5)

########################
## Removing constant variables

reduce.dimensions<-function(variables, effects, num) {
    variables <- remove.constants(variables)     ######### remove constant variables
    variables <- scale(variables)
    variables <- remove.effects(variables, effects)
    reduced.variables <- compute.pcs(variables, num)
    reduced.variables
}

new.vars <- remove.constants(variables)
identical(new.vars[,4], variables[,5])

new.vars <- reduce.dimensions(variables, effects, 5)

########################
## Identifying problems using browser()

variables[3,4] <- NA

new.vars <- reduce.dimensions(variables, effects, 5)

remove.constants <- function(variables) {
    ss <- rep(NA,ncol(variables))
    for (i in 1:ncol(variables))
        ss[i] <- var(variables[,i])
    browser()                               ######## start the browser
    is.constant <- ss < 2e-16
    if (any(is.constant)) {                 ## where the error was generated
        warning("Omitting variables with zero variance: ", sum(is.constant))
        variables <- variables[,!is.constant,drop=F]
    }
    variables
}

new.vars <- reduce.dimensions(variables, effects, 5)

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

########################
## Catching errors with stopifnot()

new.vars <- reduce.dimensions(variables, effects, 55)

reduce.dimensions <- function(variables, effects, num) {
    variables <- remove.constants(variables) 
    stopifnot(num <= ncol(variables)) ###### added to catch cases where 'num' is too large
    variables <- scale(variables)
    variables <- remove.effects(variables, effects)
    reduced.variables <- compute.pcs(variables, num)
    reduced.variables
}

new.vars <- reduce.dimensions(variables, effects, 55)

########################
## system.time() -- How long did that take anyway?

n <- 1000               ## 1000 observations/samples
n.vars <- 5e4           ## 50K variables
n.effs <- 50            ## 50 effects to remove
variables <- matrix(rnorm(n.vars*n),nrow=n)
effects <- matrix(rnorm(n.effs*n), nrow=n)
system.time(new.vars <- reduce.dimensions(variables, effects, 5))

########################
## Rprof() -- Oh, that's what took so long!

Rprof()
new.vars <- reduce.dimensions(variables, effects, 5)
profile <- summaryRprof()
profile$by.total
