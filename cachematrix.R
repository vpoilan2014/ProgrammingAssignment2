

## Creates a special matrix, which can cache its own inverse matrix and returns
#  a list of  access functions(get,set) to access the matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    #x is the matrix for which the special inverse caching Matrix is constructed.
    
    #inverse is the cached inverse matrix availble to outside environment, 
    inverse <- NULL
    
    #sets a new matrix as the cacheMatrix x. Since x is changed, the cached 
    #inverse matrix is cleared, so that it will be calculated the next access
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #Return this special caching matrix x
    get <- function() x
    
    #access methods to get and set the inverse
    #set method stores it in the inverse variable which is
    #the cache available outside the scope of this function
    setInverse <- function(inv) inverse <<- inv
    #returns the cached inverse
    getInverse <- function() inverse
    
    #Return the list of access functions.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the the inverse of the matrix if it is not 
#calculated and cached previously.

cacheSolve <- function(x, ...) {
   
    #Returns the cached inverse matrix,if it is already calculated
    # and cached for the matrix x
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #Calculate the inverse of the matrix x, since the cache is not found.
    data <- x$get()
    inv <- solve(data)
    
    # Set the inverse to cache it for later use
    x$setInverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
