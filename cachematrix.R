# Mattrix inversion is a costly operation.  Here we define two functions
# that together allow us to cache the inverse of a matrix and help our 
# code run faster.

# makeCacheMatrix 
#
# Creates a "special" matrix objects that allows caching of the inverse
#
# Args:
#   x: The matrix for which we want to cache the inverse
#
# Methods:
#   get(): Prints the matrix x 
#   set(y): Reset the matrix to y 
#   setInverse(i): Caches the newly calculated value of the inverse
#   getInverse(): Returns the existing cached value of the inverse
#
# Returns:
#   The "special" matrix list object

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL         # Sets the initial value for inverse
    set <- function(y) {    # This anon function rewrites the matrix
        inverse <<- NULL    # and erases the old inverse
        x <<- y             # all assignments have global scope 
    }
    get <- function() x	     # One-line function definition
    setInverse <- function(inv) inverse <<- inv  # Set inverse
    getInverse <- function() inverse             #Return inverse
    list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


# cacheSolve 
# 
# Returns the inverse of a matrix if it is cached, otherwise calculates,
# caches, and returns the inverse of the matrix. 
# 
# Args:
#   x: The "special" object created by makeCacheMatrix function      
#
# Returns: 
#   The inverse of a matrix   

cacheSolve <- function(x, ...) {
    i <- x$getInverse()   # Check to see of inverse is already cached
    if (!is.null(i)){     # If so, return it and exit.
        message("Getting cached matrix...")
        return(i) # This line exists the function if the inverse was cached.
    }
    
    data <- x$get()   # Read the matrix 
    i <- solve(data)  # Calculate the inverse of the matrix
    x$setInverse(i)   # Cache the inverse of the matrix
    i                 # Return the inverse
}
