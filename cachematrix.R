## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


## makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

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


## The following function computes the inverse of the 
## special "metrix". However, it first checks to see if 
## the inverse has already been stored. If so, it gets 
## it from the cache and skips the computation. Otherwise, 
## it computes the inverse and sets the value in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


