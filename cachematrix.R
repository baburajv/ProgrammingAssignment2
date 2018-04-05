## function makeCacheMatrix accepts a square matrix as input.
##  the inv variable stores the inverse of the matrix and the functions
## set, get are to set and get the values of x and setInverse, getInverse are to set and get inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## the cacheSolve function takes a square matrix as input, sets caches the value,  and returns the invers from cache if it ## exists else compute the inverse and stores that in inv variable using setinverse 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    f <- makeCacheMatrix()
    f$set(x)
    
    inv <- f$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- f$get()
    inv <- solve(mat, ...)
    f$setInverse(inv)
    inv
}