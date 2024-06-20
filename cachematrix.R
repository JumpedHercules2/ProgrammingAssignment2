## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
    inv <- NULL
    set <- function(matrixInput) {
        matrix <<- matrixInput
        inv <<- NULL
    }
    get <- function() matrix
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
    inv <- mat$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setInverse(inv)
    inv
}