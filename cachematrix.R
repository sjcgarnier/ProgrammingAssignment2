## This code will cache a time-consuming computation of a matrix inversion  
## The `<<-` operator is  used to assign a value to an object in an environment
## that is different from the current environment. The two functions that are used
## create a special object that stores a matrix and caches its inversion.
## 
## The function 'makeCacheMatrix' checks to see if the inversion of the matrix 
# has already been calculated. If so, it get`s the inversion from the
## cache and skips the computation. Otherwise, it calculates the inversion of
## the data and sets the value of the inversion in the cache via the `cacheSolve`
## function. 
#
# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a database containing a function to
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The function 'cacheSolve' computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}
