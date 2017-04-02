## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function is a matrix inversion version of the
## makeVector example program.
## To me, it looks more like a class in C++ or Java.
## It takes a matrix as an argument and assigns it to
## a local variable x that will hold the source matrix,
## and set the inverse, mtx_iv that will hold x^(-1), to null.
## get and set are getter and setter, respectively, for the data
## matrix x. Note that the inverse matrix mtx_inv is set to null
## when a new matrix is assigned, since the new inverse is
## probably different then the old one (might me good idea to check
## if the local data x is different from the new data).
## setinv calculates the inverse of x and stores it in mtx_inv
## getinv returns the inverse matrix, mtx_inv.
makeCacheMatrix <- function(x = matrix()) {
    mtx_inv <- NULL
    set <- function(y) {
        x <<- y
        mtx_inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mtx_inv <<- solve
    getinv <- function() mtx_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function

## This function takes a 'makeCacheMatrix' variable as an
## input argument, and resturns the inverse matrix of the
## data matrix that is stored in x (one may think of x as
## a C++/Java class).
## First, the function tests if the inverse is already calculated
## and stored in x though a call to getinv (x$getinv actually...).
## If the getinv returns a non-null, that value is assumes to
## be the inverted matrix and returned.
## Otherwize, the data is copied from x to a local variable data,
## and if data is a square matrix, the function calculate it's
## inverse, stores it in x for later use, and returnes the resulted
## inverse.
## If the function is not inverse, an error is printed and a null
## is returned.
##
## The function is tested by multiplying a matrix by it's inverse
## as follow
## > m <- matrix (1:4,2,2)
## > m %*% cacheSolve(makeCacheMatrix(m))
## The result should be a 2x2 I matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    mtx_inv <- x$getinv()
    if(!is.null(mtx_inv)) {
        message("getting cached data")
        return(mtx_inv)
    }
    data <- x$get()
    if (ncol(data) != nrow(data)) {
        message("getting cached data")
        return(NULL)
    }
    mtx_inv <- solve(data, ...)
    x$setinv(mtx_inv)
    mtx_inv
}
