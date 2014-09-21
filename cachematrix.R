## In this code we write 2 functions "makecachematrix" and "cachesolve" that cache the inverse of a matrix. This assignment assumes that the matrices provided are always invertible.

## 1st Function: caching the inverse of a matrix
##  The makeCacheMatrix function creates a special "matrix"  that can cache its inverse. The sequence of operations is as follows:
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse
## makeCacheMatrix returns a list containing the functions set,get,setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {

## set variable m to NULL

    m <- NULL
    
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    
}

##2nd Function: computing the inverse of a function
## The cacheSolve function solves for the inverse of the special "matrix" returned by the function `makeCacheMatrix`.
## It checks to see if the inverse had been computed. If so, assuming the matrix has not been changed, `cacheSolve` prints "getting cached data" and
## gets the inverse from the cache and skips the computation of the inverse.
## Or else, it computes the inverse of the matrix and sets the inverse matrix in the cache via the setinverse function.
## cacheSolve returns a matrix that is the inverse of the given matrix 'x'

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}


## End of Code
## what follows below is a SOLUTION set for a trial that we use to demonstrate the functionality of the code.

> source("cachematrix.R")
> m<-makeCacheMatrix(matrix(3:6,2,2))
> cacheSolve(m) ## calculates and caches the matrix inverse
[,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
> cacheSolve(m) ## Returns cached matrix inverse because it was already computed
getting cached data
[,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
> m$get()   ## gets the original matrix
[,1] [,2]
[1,]    3    5
[2,]    4    6
> m$getinverse() ## gets the matrix inverse already calculated
[,1] [,2]
[1,]   -3  2.5
[2,]    2 -1.5
> m$set(matrix(c(85,65,45,25), nrow=2, ncol=2)) ##resets m to a new matrix
> cacheSolve(m) ## calculates and caches the new matrix inverse
[,1]     [,2]
[1,] -0.03125  0.05625
[2,]  0.08125 -0.10625
> m$get() ## gets the new matrix
[,1] [,2]
[1,]   85   45
[2,]   65   25
> m$getinverse() ## gets the matrix inverse already calculated
[,1]     [,2]
[1,] -0.03125  0.05625
[2,]  0.08125 -0.10625
> cacheSolve(m) ## Returns cached matrix inverse because it was already computed
getting cached data
[,1]     [,2]
[1,] -0.03125  0.05625
[2,]  0.08125 -0.10625
>

