## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix constructs an object for caching matrix inverse
## it has methods for getting and setting source matrix and cached inverse value  
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve is a function that gets matrix inverse from cache if already solved 
## and if not it solves inverse and stores value to cache 
cacheSolve <- function(x, ...) {
    ##check for matrix to be square
    data <- x$get()
    if (nrow(data) != ncol(data)){
        message("Error: Matrix should be square")
    }
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
