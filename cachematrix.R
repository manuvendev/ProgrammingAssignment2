## makeCacheMatrix: is in charge of initializing a matrix object
## and its methods, both setters and getters, for itself and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: retrieves the inverse of a matrix.
## If the inverse of this matrix has been previously computed
## it gets returned, otherwise it is computed and then set for future computations.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv  
}


## Example
# x <- makeCacheMatrix()
# x$set(rbind(c(1, -1/4), c(-1/4, 1)) )
# cacheSolve(x) # Inverse will be computed and returned
# cacheSolve(x) # Cached inverse returned