## This is a pair of function that works together to cache the inverse of 
## a matrix in the parent environment. It consist of makeCacheMatrix() 
## function and cacheSolve() function

## makeCacheMatrix() function creates a special "matrix" object that can 
## cache its inverse. The function take a matrix class argument x (empty 
## matrix by default) and return a list of setter and getter of the matrix 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix() function. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve() function should retrieve the inverse from the cache.
## The function takes a matrix and an ellipsis arguments.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message('getting cached data')
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
