## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that is used to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y){
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixInverse) mat_inv <<- matrixInverse
    getinverse <- function() mat_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$getinverse()
    if(!is.null(mat_inv)){
        message("getting cached data")
        return(mat_inv)
    }
    data <- x$get()
    mat_inv <- solve(data, ...)
    x$setinverse(mat_inv)
    mat_inv
}
