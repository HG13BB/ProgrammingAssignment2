## Put comments here that give an overall description of what your
## functions do

## Creates a matrix and calculates/caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## define value of matrix inverse to NULL
        matrixinverse  <- NULL
        ## create another function where value will be cached
        set <- function(y) {
                x <<- y
                ## change the value of inverse of matrix.
                matrixinverse <<- NULL
        }
        ## gets value of inverse
        get <- function() x
        #calculate the inverse of matrix with solve function
        setinverse <- function(solve) matrixinverse  <<- solve
        # gets the inverse     
        getinverse <- function() matrixinverse 
        ## pass the value of the function makeCacheMatrix        
        list(set = set, get = get,
             getinverse = getinverse,
             setinverse = setinverse)
}


## Looks for and retrieves cached inverse from makeCacheMatrix, 
## if no inverse if cached, calculates inverse

cacheSolve <- function(x, ...) {
        # get the cache of the matrix
        matrixinverse <<- x$getinverse()
        #if the inverse has already be calculated in makeCacheMatrix function, get it
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        #if the inverse has not been calculated, it is calculated and then retrieved.
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}
       