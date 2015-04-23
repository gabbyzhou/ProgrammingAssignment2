## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than computing it repeatedly.
# makeCacheMatrix function creates a special "matrix" object that can cache its 
# inverse.

# The function is a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse of the matrix
# 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        gerinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# The following function calculates the mean of the special "vector" 
# created with the above function. It first checks to see if the mean 
# has already been calculated. If so, it gets the mean from the cache 
# and skips the computation. Otherwise, it calculates the mean of the 
# data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        x$setinv(i)
        i
}
