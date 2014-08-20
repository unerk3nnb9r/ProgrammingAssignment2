## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a structure (a list) of elements used to keep track of the current
# value of a matrix and its value during a prior invocation, and functions ("methods") to
# manage this structure.

# cacheSolve uses the structure to return without recalculation the inverse of a matrix that
# remains unchanged, or to update the structure to reflect a change.

# Other than described below, arguments are not checked: matrix is assumed invertible, etc.


## makeCacheMatrix(x) -> y    (x is an inversible matrix, y is a "cache" structure)

# Besides the functions presented in the vector example, here the return list contains the
# name of the matrix passed. That name will allow cacheSolve to check - as requested - if
# the matrix has changed. That check *will work for individual matrixes*. Checking equality
# of expresions that evaluate to inversible matrixes is not implemented.

makeCacheMatrix <- function(x = matrix()) {
    getArg <- deparse(substitute(x))
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set,
        get = get,
        setSolve = setSolve,
        getSolve = getSolve,
        getArg = getArg
    )
}


## cacheSolve(y)    (y is a "cache" structure returned by an invocation to makeCacheMatrix)

# This function receives a list produced by makeCacheMatrix. First it checks if the current
# value of the original matrix remains the same. That check is done with "identical"
# instead of "if". (Although not relevant for this problem, "identical" detects changed
# dimensions besides content.)
#
# If the matrix has changed, cacheSolve reports it before updating the "cache" structure via
# the "set" function in the structure. This action is also available to the user. 
# 
# Next, cacheSolve checks if the inverse exists. If it does, it corresponds to the current
# content of the matrix referenced, and it is returned directly from the cache.
#
# If the inverse does not yet exist or was invalidated, it is now calculated, the structure
# is updated and the inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    current <- get(x$getArg, envir=1)
    data <- x$get()
    if(!identical(current, x$get())) {
        message("matrix has changed. updating cache")
        x$set(current)
        data <- current
    }
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
