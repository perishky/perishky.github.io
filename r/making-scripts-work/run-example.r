########################
## Example: dimension reduction

source("functions-1.r")

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

source("functions-2.r") ## adds remove.constants()

new.vars <- remove.constants(variables)
identical(new.vars[,4], variables[,5])

new.vars <- reduce.dimensions(variables, effects, 5)

########################
## Identifying problems using browser()

variables[3,4] <- NA

new.vars <- reduce.dimensions(variables, effects, 5)

source("functions-3.r") ## call browser()

new.vars <- reduce.dimensions(variables, effects, 5)

source("functions-4.r") ## handle missing values

########################
## Catching errors with stopifnot()

new.vars <- reduce.dimensions(variables, effects, 55)

source("functions-5.r") ## checks for invalid input

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
