source("functions-4.r")

reduce.dimensions <- function(variables, effects, num) {
    variables <- remove.constants(variables) 
    stopifnot(num <= ncol(variables)) ###### added to catch cases where 'num' is too large
    variables <- scale(variables)
    variables <- remove.effects(variables, effects)
    reduced.variables <- compute.pcs(variables, num)
    reduced.variables
}
