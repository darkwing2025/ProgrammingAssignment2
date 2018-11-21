## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A list of functions that set/get the matrix and its inverse
## If matrix has been cached, m would have a prior calculated value.
## m set to NULL for every time makeCacheMatrix is run
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinv <- function(inv) {m <<- inv}
        getinv <- function() {m}
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## gets the cached inverse, or solves for the inverse of a new matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
