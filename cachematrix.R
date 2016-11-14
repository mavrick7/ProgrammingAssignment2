## Put comments here that give an overall description of what your
## functions do

## Here the given two functions are used for taking a matrix as an
## argument and stores it in a special object that is created by these
## functions and caches its inverse.


## Write a short comment describing this function

## Here the function "makeCacheMatrix" is used to crate a special matrix
## object that can store its inverse for later use.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
		
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
		
        get <- function() x
        
		setInverse <- function(inverse) i <<- inverse
        
		getInverse <- function() i
        
		list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## Write a short comment describing this function

## This function "cacheSolve" get us the inverse of the matrix created by
## the "makeCacheMatrix" function , if inverse already exist/created it gets
## from cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
        if (!is.null(i)) {
                message("Inverse exist,getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
