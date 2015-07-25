## ----------------------------------------------------------------------------
## Course:  Coursera - R Programming
## Project: Programming Assignment 2
## Title:   Caching the Inverse of a Matrix
## 
## Author:  DataExpert (Coursera student)
## Date:    2015/07/25
## 
## Comment: -
## ----------------------------------------------------------------------------
    
## This function creates a special "matrix" object that caches its inverse. 
## The resulting object is a list containing functions to set and get the
## matrix and to set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse with NULL object to state it has not yet been determined
    matrix_inverse <- NULL 
    
    # function to set the matrix (and uninitialized inverse)
    set <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    
    # function to get the matrix
    get <- function() x
    
    # functions to set and get the inverse matrix
    set_inverse <- function(inv) matrix_inverse <<- inv
    get_inverse <- function() matrix_inverse
    
    # special "matrix object" with all of these functions
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function calculates the inverse matrix of the special matrix object created
## with the above function. It first checks, whether the inverse has already been cal-
## culated. If so, it gets the pre calculated inverse matrix from the cache and skips
## the computation. Otherwise, it calculates the inverse matrix and sets it via the 
## set_inverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the actual inverse matrix from x object
    matrix_inverse <- x$get_inverse()
    
    # check if it yet has been calculated
    if(!is.null(matrix_inverse)){
        message("getting cached data")
        return(matrix_inverse)
    }
    
    # the inverse matrix hasn't yet been calculated...
    data <- x$get()
    matrix_inverse <- solve(data, ...)
    
    # set result for next time in cache
    x$set_inverse(matrix_inverse)
    
    #return result
    matrix_inverse
}


## Test
#print("initialization of matrix: my_matrix <- makeCacheMatrix(matrix(1:4, nrow = 2, byrow = TRUE))")
#my_matrix <- makeCacheMatrix(matrix(1:4, nrow = 2, byrow = TRUE))
#print("1st call of cacheSolve")
#print(cacheSolve(my_matrix))
#print("2nd call of cacheSolve (access to cached data...)")
#print(cacheSolve(my_matrix))
