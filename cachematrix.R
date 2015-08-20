# This function follows Google's R Style Guide
# URL: https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

makeCacheMatrix <- function(x = matrix()) {
    # Initializes a matrix object and its methods, both setters and getters,
    # for itself and its inverse.
    #
    # Args:
    #   x: A initialization matrix object
    #
    # Returns:
    #   A list with all the methods that can be accessed.
    
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

cacheSolve <- function(x, ...) {
    # Returns the inverse of a matrix object, of the makeCacheMatrix type, if it is cached
    # or computes it if it hasn't been computed yet.
    #
    # Args:
    #   x: A makeCacheMatrix object
    #
    # Returns:
    #   Inverse of x
    
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


# Example
# x <- makeCacheMatrix()
# x$set(rbind(c(1, -1/4), c(-1/4, 1)) )
# cacheSolve(x) # Inverse will be computed and returned
# cacheSolve(x) # Cached inverse returned