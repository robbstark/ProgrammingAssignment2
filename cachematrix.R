# mattrix inversion is a costly operation.  Here we define two functions
# that together allow us to cache the inverse of a matrix and help our 
# code run faster.


# The function makeCacheMatrix takes a matrix as an input and creates a
# list object that includes methods for getting and setting the matrix 
# and the cached value for inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {    # This anon function allows the matrix to be  
        inverse <- NULL     # changed after the initial construction
        x <<- y
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


# This function first checks to see if a cached value of the matrix is
# present in our object.  If it is there, it will return it, if 
# not, it will calculate it and sets the cached value for future use.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)){
        message("Getting cached matrix...")
        return(i) # This line exists the function if the inverse was cached.
    }
    
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
