########### Programming Assignment 2:  Lexical Scoping ###########

## The following program provides an alternative to cache the
## inverse of a matrix rather than compute it repeatedly.
## For this purpose, by taking advantage of the scoping rules of 
## the R language, the next functions create an especial R object.


## The first function, makeCacheMatrix, creates a special "matrix",
## which is really a list containing functions to set and get the 
## value of the vector and to set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function computes the inverse of the special 
## "matrix" provided by makeCacheMatrix, returning the cached inverse 
## when it'd been evaluated. Otherwise, it calculates the inverse and
## set it's value with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
