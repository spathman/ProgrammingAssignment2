## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - creates a special "matrix" that contains a
## list of functions that operate on the created matrix to set and
## get the matrix as well to set and get inverted value of the
## matrix. The calculated inverse is stored in a member 'inv'

## Examples:
##  a <- makeCacheMatrix() # creates an empty special matrix
##  b <- makeCacheMatrix(matrix(1:9, nrow = 3, ncol = 3))

##  a$get() # returns an empty matrix
##  b$get() # returns a 3 x 3 matrix

##  a$set(matrix(1:6, nrow = 2, ncol = 3)) # sets 'a' to the new value

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # holds the inverse of this matrix
    set <- function(y) {
        x <<- y # reset this matrix to 'y'
        inv <<- NULL # reset the inverse
    }
    get <- function() x # returns this matrix
    setinverse <- function(newInv) inv <<- newInv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() # get the iverse of matrix x (NULL or cached value)
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if no cached value
    data <- x$get() # get the matrix associated with 'x'
    if (nrow(data) != ncol(data)) {
        message("It is not a square matrix!")
        return(NULL)
    }else if (det(data) == 0) {
        message("It's a singular matrix!")
        return(NULL)
    }
    # matrix has an inverse
    inv <- solve(data, ...) # calculate the inverse of 'x'
    x$setinv(inv) # cache the inverse
    inv # return the the inverse of 'x'
}
