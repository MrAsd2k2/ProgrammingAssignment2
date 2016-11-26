## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special list containing a function to:
## - set the value of the matrix;
## - get the value of the matrix;
## - set the value of the inverse of the matrix;
## - get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse 
         ) # create the list of functions
}

## The following function calculates the inverse of a square matrix.
## It first checks if the inverse has already been calculated and retrieve it from the 
## cache skipping any calculation. 
## Differently it calculates the inverse and set the value in the cache.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) { # if inver != NULL we retrieve the cached inverse...
            message("getting cached data")
            return(inver)
        } #... differently, compute the inverse and return it
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}

## Unit Test:
## > x <- matrix(rnorm(16),4,4)
## > ix <- makeCacheMatrix(x)
## > ix$get()
## [,1]       [,2]       [,3]         [,4]
## [1,] -0.5856595  2.4228733  0.2973935  0.076499341
## [2,] -0.5907215 -0.5234245 -1.2204930 -0.626291762
## [3,] -1.1428690  1.1553864 -0.3039428 -0.004988406
## [4,]  0.6873107 -1.7294594 -0.2458384 -0.026873392
## > cacheSolve(ix)
## [,1]       [,2]        [,3]       [,4]
## [1,]  1.960978  0.1436017 -0.54844498  2.3373611
## [2,]  1.160215  0.1002169 -0.05821295  0.9779587
## [3,] -3.013309 -0.1334717 -1.50662239 -5.1875947
## [4,]  3.052963 -1.5557978  3.50199473  7.0874413
## > cacheSolve(ix)
## getting cached data
## [,1]       [,2]        [,3]       [,4]
## [1,]  1.960978  0.1436017 -0.54844498  2.3373611
## [2,]  1.160215  0.1002169 -0.05821295  0.9779587
## [3,] -3.013309 -0.1334717 -1.50662239 -5.1875947
## [4,]  3.052963 -1.5557978  3.50199473  7.0874413
