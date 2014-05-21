## These are two functions used to create objects to store a matrix and 
## cache its inverse. These functions make use of the <<- operator which
## is used to assign a value to an object that is in a different from the
## current environment

## Creates a Cache Matrix Object. This is actually a list containing functions to:
## set the matrix, get the matrix, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of x and stores it in a Cache Matrix object.
## If the inverse was previously computed, use the cached result.
## The inverse is returned

cacheSolve <- function(x, ...) {
        
    inv<-x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
    
}
