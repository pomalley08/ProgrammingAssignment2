################################################################################
# Course: R Programming
# Title:  Week 3 Programming Assignment 2
# Author: Patrick O'Malley
# Date:   6/20/17
################################################################################

# Create a set of functions that can calculate the inverse of a matrix and cache
# the value and then subsequently returns that cached value


# Function to create matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# Function to compute the inverse of the matrix, or retrives cached value if
# already saved
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
