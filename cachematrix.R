## makeCacheMatrix creates a structure (a list) of elements used to keep track of the current
## value of a matrix and to cache and access this matrix and its inverse.

## cacheSolve uses this structure to return the inverse of the matrix without re-computation,
## updating the structure to reflect a change when required.

# Arguments are not checked for validity.

## makeCacheMatrix(x)   x is an inversible matrix

# Input is an inversible matrix. Output is a list containing functions to manage a cache, plus
# the name of the matrix passed. That name will allow cacheSolve to check - as requested - if
# the matrix has changed. That check works for a single matrix as argument. Checking change in
# an expresion that evaluates to an inversible matrix is not implemented. The check is made in
# the original scope.

makeCacheMatrix <- function(inversibleMatrix = matrix()) {
    # Obtain argument's name
    getArg <- deparse(substitute(inversibleMatrix))
    solvedMatrix <- NULL
    # Cache matrix
    set <- function(y) {
        inversibleMatrix <<- y
        solvedMatrix <<- NULL
    }
    # Get matrix from cache
    get <- function() {
        inversibleMatrix
    }
    # Cache inverse
    setSolve <- function(solve) {
        solvedMatrix <<- solve
    }
    # Get inverse from cache
    getSolve <- function() {
        solvedMatrix
    }
    # Return cache structure as list
    list(set = set,
        get = get,
        setSolve = setSolve,
        getSolve = getSolve,
        getArg = getArg
    )
}


## cacheSolve(y)    y is a cache returned by an invocation to makeCacheMatrix

# Input is a cache produced by makeCacheMatrix. First it checks if the matrix has changed.
# It uses "identical" instead of "all ( ... == ... )". Faster, screens changed dimensions and
# content.
#
# If the matrix has changed, reports it, and updates the cache structure via the "set" function.
# This action is also available to the user. 
# 
# Checks if the inverse exists. If it does, it corresponds to the current content and is
# returned directly from the cache.
#
# If the inverse does not exist, it is calculated, the cache is updated and the inverse is returned.

cacheSolve <- function(cache, ...) {
    # Get current content
    current <- get(cache$getArg, envir=1)
    # Compare, update if required
    cached <- cache$get()
    if(!identical(current, cached)) {
        message("matrix has changed. updating cache")
        cache$set(current)
        cached <- current
    }
    # Return cached inverse if it exists
    solvedMatrix <- cache$getSolve()
    if(!is.null(solvedMatrix)) {
        message("getting cached data")
        return(solvedMatrix)
    }
    # No cached inverse: solve and return it
    solvedMatrix <- solve(cached, ...)
    cache$setSolve(solvedMatrix)
    solvedMatrix
}

# Test run:
#
# > a <- matrix(1:4, ncol = 2)
# > b <- c(1, 2, 0, 3, 4, 0, 0, 0, 1); dim(b) <- c(3, 3)
# > m <- makeCacheMatrix(a)
# > cacheSolve(m)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > a <- b
# > cacheSolve(m)
# matrix has changed. updating cache
#      [,1] [,2] [,3]
# [1,]   -2  1.5    0
# [2,]    1 -0.5    0
# [3,]    0  0.0    1
# > cacheSolve(m)
# getting cached data
#      [,1] [,2] [,3]
# [1,]   -2  1.5    0
# [2,]    1 -0.5    0
# [3,]    0  0.0    1
# > m <- makeCacheMatrix(a)
# > cacheSolve(m)
#      [,1] [,2] [,3]
# [1,]   -2  1.5    0
# [2,]    1 -0.5    0
# [3,]    0  0.0    1
# > cacheSolve(m)
# getting cached data
#      [,1] [,2] [,3]
# [1,]   -2  1.5    0
# [2,]    1 -0.5    0
# [3,]    0  0.0    1
# > 
