## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions work together to make this possible.

## The makeCacheMatrix function creates a special "matrix" object which is really a list
## containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Input
## x : the matrix to store in the object

## Returns a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        get <- function() x
        setInv <- function(inv) invX <<- inv
        getInv <- function() invX
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse is returned from the cache.

## Input
## x : the special "matrix" object
## ... any additional arguments to pass to the solve function

## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        invX <- x$getInv()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- x$get()
        invX <- solve(data, ...)
        x$setInv(invX)
        invX
}
