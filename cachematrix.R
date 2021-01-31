## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a matrix opbject that allows a user to cache its inverse

## The cacheSolve function calculates the inverse of the matrix object that is returned in the
## makeCacheMatrix function. If the inverse has already been calculated and the matrix is unchanged,
## the cacheSolve retrieves the inverse from the cache

## Write a short comment describing this function

## This function creates a matrix object that does the following four things:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the matrix inverse
## 4. gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<-y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setINverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function calculates the inverse of the matrix created in the function above
## If checks to see if the inverse has already been calculated; if it has:
## it returns the inverse from the cache and skips the calculation 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInverse(m)
        m
}