### Operating on a list of data frames.

data(mtcars)
data(iris)
myList <- list(A = mtcars, B = iris)

## All three approaches yield identical results.

# helper function and call
removeCols <- function(df, vec) {
    res <- df[, - vec]
}
lapply(myList, removeCols, 1:2)

# using anonymous function
yy <- lapply(myList, function(df, vec) { df[, - vec] }, 1:2)

# Use the '[' operator
zz <- lapply(myList, "[", - (1:2))