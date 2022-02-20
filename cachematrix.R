## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix",
makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y) {
        x <<- y
        inve <<- NULL
    }
    get <- function() x
    setinverse <- function(invers) inve <<- invers
    getinverse <- function() inve
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##test 
##my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##> my_matrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    inve <- x$getinverse()
    if(!is.null(inve)) {
        message("getting cached data")
        return(inve)
    }
    data <- x$get()
    inve <- solve(data, ...)
    x$setinverse(inve)
    inve
}
##test
##my_matrix <- (matrix(1:4, 2, 2))
##m1 <- makeCacheMatrix(my_matrix)
##cacheSolve(m1)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

