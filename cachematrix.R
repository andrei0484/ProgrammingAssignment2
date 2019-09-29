## This is a set of functions that computes and cashes the inverse of
## a supplied invertible matrix. It also provides the means of accessing 
## and/or updating the cahed data.
## The inverse matrix gets recalculated only if the original matrix changes.


## The first function creates a special "matrix" object with four functions 
## that get and set data of the original matrix and its cashed inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setx <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getx <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = setx, get = getx,
             setinv = setinverse,
             getinv = getinverse)
}


## The second function computes and returns the inverse of the special "matrix"
## created by the above function. If the inverse has already been calculated 
## and the matrix has not changed, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
