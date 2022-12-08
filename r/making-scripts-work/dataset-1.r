n <- 50                 ## 50 observations/samples
n.vars <- 20            ## 20 variables
n.effs <- 5             ## 5 effects to remove
variables <- matrix(rnorm(n.vars*n),nrow=n)
effects <- matrix(rnorm(n.effs*n), nrow=n)
