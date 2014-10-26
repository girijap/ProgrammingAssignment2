## The following functions show the effects of lexical scoping on variables 
## by initializing a matrix and caching the inverse of a matrix using the
## super assignment operator <<- in the makeCacheMatrix() function. 
## The inverse of this matrix is computed by using the R solve() function in the
## cacheSolve() function below.

## This function declares and initializes matrix m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## m initialized to NULL using assignment operator
    set <- function(y) {
        x <<- y
        m <<- NULL 
    } ## super assignement operator used to update the value of m
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of an invertible matrix using the R solve() function
## It computes, caches, and returns new matrix inverse

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
    m <- x$getinverse() ## gets the matrix inverse
    if(!is.null(m)) { ## if inverse was calculated and stored in the cache before, the cached value is returned
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ## The inverse of the  matrix is computed using the solve() function
    x$setinverse(m) ## If inverse of the matrix was not cached the value is computed and stored
    m
}
