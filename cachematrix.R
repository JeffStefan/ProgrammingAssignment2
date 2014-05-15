#####################################################################
# R Programming Assignment 2 Notes
# Filename: cachematrix.R
# Author: Jeff Stefan
# Course: R Programming Johns Hopkins Data Science
#####################################################################
## Put comments here that give an overall description of what your
## functions do.
## makeCacheMatrix 
##  1. sets the value of the matrix
##  2. gets the value of the matrix
##  3. sets the inverse of the matrix
##  4. gets the inverse of the matrix
##
## cacheSolve
## 1. checks to see if the matrix inverse is already solved
## 2. if so, the cached mean is returned
## 3. if the matrix inverse is not solved
## 4. solves the matrix inverse and returns it
## 
## Assumptions:
## The input matrix is always invertable
## Inverse Matrix notes:
##  The inverse of a square matrix is the equivalent of
##  dividing by that matrix. This is useful for multiple 
##  regression, allowing it to solve for beta weights.
## 
## Example matrix to use:
## 
## CAUTION 
## The code should check the det(x) before trying to compute
## the inverse. If det(x) returns 0, then the inverse can't be
## calculated. 
############################################################
## Write a short comment describing this function
## makeCacheMatrix 
##  1. sets the value of the matrix
##  2. gets the value of the matrix
##  3. sets the inverse of the matrix
##  4. gets the inverse of the matrix
##
##########################################################
makeCacheMatrix <- function(x = matrix()) {
    ## allocate variable inv to hold inverse matrix value
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()  x  
    # set up call to solve inverse matrix
    setInv <- function(solve) inv <<- solve
    getInv <- function()  inv 
    ## make a list containing the function calss
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

#################################################################
# Write a short comment describing this function
## cacheSolve
## 1. checks to see if the matrix inverse is already solved
## 2. if so, the cached mean is returned
## 3. if the matrix inverse is not solved
## 4. solves the matrix inverse and returns it
## 
## Assumptions:
## the input matrix is always invertable per prog2 instructions
#################################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    ## if inv is already computed return it
    if(!is.null(inv)) {
        message("getting cached matix data")
        return(inv)  
    }
    ## if inv is not computed, compute it with solve()
    data <- x$get()
    inv <- solve(data,...)
    x$setInv(inv)
    inv
}

