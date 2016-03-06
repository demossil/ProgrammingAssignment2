## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##setMatrix: set the value of the matrix
##getMatrix: get the value of the matrix
##setInverse : set the inverse of the matrix
##getInverse : set the inverse of the matrix
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
##Calculates the inverse of the matrix created with the above function, it 
##first checks to see if the inverse has already been calculated. If so, it 
##gets the mean from the cache and skips the computation. Otherwise, it 
##calculates the inverse of the data and sets the value of the inverse in 
##the cache via the setmean function.

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
