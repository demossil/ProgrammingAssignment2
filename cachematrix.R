## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    setMatrix <- function(matrix){
        x <<- matrix;
        inv_x <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inv_x <<- inv
    getInverse <- function() inv_x
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getInverse()
    if(!is.null(inv_x)){
        message("getting cached data")
        return(inv_x)
    }
    matrix <- x$getMatrix()
    inv_x <- solve(matrix)
    x$setMatrix(inv_x)
    inv_x
}
