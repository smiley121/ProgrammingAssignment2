## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Here I implement a  pair of functions that cache the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 
## It is really a list containing a function to
## 1. set the value of the "matrix"
## 2. get the value of the "matrix"
## 3. set the cached inverse of the "matrix" to a value supplied; no check is
##    performed that this is the matrix inverse.
## 4. get the cached inverse of the "matrix"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  
        set <- function(thismatrix) {
                x <<- thismatrix  ## initialise the matrix to the value supplied
                inv <<- NULL      ## initialise the inverse as an empty variable
        }
        
        get <- function() {
                x                 ## return the matrix
        }
        
        setinverse <- function(inverse) {
                inv <<- inverse   ## set the inverse to the value supplied
        } 
        
        getinverse <- function() {
                inv               ## returns the inverse value 
        }
        
        list(set = set,           ## returns a list of four functions
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then  cachesolve retrieves the inverse from
## the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   ## looks up the inverse for x          
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)      ## simply returns the cached value of 
                                 ## the inverse if it exists          
        }
        
        inv <- solve(x$get())    ## finds the inverse of x if it's not cached
        x$setinverse(inv)        ## caches this
        inv                      ## returns this inverse
}
