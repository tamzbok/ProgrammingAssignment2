## Put comments here that give an overall description of what your
## functions do

## Comments:  These functions provide for caching computation for 
##	inverse matrix.  This is an expensive computation in R
##	and an ability to cache the results of an inverse matrix
##	computation can significantly improve the time to return
##	the answer for succeeding calls.


## Write a short comment describing this function

## Function:  makeCacheMatrix
## Comments:
##	This function accepts a matrix as an argument and
##	returns a special matrix that can cached its
##	inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

## Function: cacheSolve
## Comments:
##	This function accepts a special matrix created from 
##	the function makeCacheMatrix.  The function will
##	check if the inverse is already cache for the matrix
##	and will return the cached inverse matrix if it 
##	exists or will compute for the inverse if not

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
