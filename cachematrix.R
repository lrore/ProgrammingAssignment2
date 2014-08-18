# Functions to create a special object that stores a matrix and cache's its inverse.

#creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL              # inverse is reset to NULL for every function call 
                                      
        set <- function(y) {
                x <<- y                # saves the input vector 
                inverse <<- NULL       # resets the inverse to NULL
        }
        get <- function() x            # returns the value of the original matrix
        
        setinv <- function(solve) inverse <<- solve          # stores inverse 
        getinv <- function() inverse         # returns the cached inverse
        
        list(set = set, get = get,   #creates a list of methods
             setinv = setinv,        # each time makeCacheMatrix() is called
             getinv = getinv)
}

#computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getinv()                  # accesses the object 'x' and gets the value of inverse
        if(!is.null(inverse)) {                # if inverse was already cached
                message("getting cached data") # generates a message
                return(inverse)                # returnes cashed inverse
        }
        data <- x$get()                 # assigns value of the original matrix to 'data'
        inverse <- solve(data, ...)     # calculates inverse
        x$setinv(inverse)               # stores inverse in 'x'
        inverse                         # returns inverse
}