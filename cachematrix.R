## The following functions show the effects of lexical scoping on variables 
## by initializing a matrix and caching the inverse of a matrix using the
## super assignment operator <<- in the makeCacheMatrix() function. 
## The inverse of this matrix is computed by using the R solve() function in the
## cacheSolve() function below.

## This function declares and initializes matrix m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of an invertible matrix using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
