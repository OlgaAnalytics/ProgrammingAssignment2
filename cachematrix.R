## I have created 2 functions that help cache the inverse of a matrix. 
## Taking the inverse of a matrix is typically a fast operation, but may take too long if the matrix 
## is particularly large. Therefore, it may make sense to cache the inverse of said function if we have 
## just computed its inverse a step prior. 

## The first function, makeCacheMatrix, must be loaded into the environment for the second function, 
## cacheSolve to work, because cacheSolve calls makeCacheMatrix within it. However, both functions take 
## x (an inversible matrix) as their main arguments. 

## In other words, to make a "special" matrix using makeCacheMatrix(), you will load the function, and then call
## makeCacheMatrix(x), where x is an inversible matrix.
## To then cache/calculate the inverse of x, you must have both makeCacheMatrix() and cacheSolve() loaded, 
## and then you will call cacheSolve(x), where x is again an inversible matrix. To save steps, you may simply 
## load both functions in and then call cacheSolve(x) without calling makeCacheMatrix(x) first.

##The first function, "makeCacheMatrix()" creates a special "matrix," which is just a function to
##1. set the value of the matrix;
##2. get the value of the matrix;
##3. set the value of the inverse;
##4. get the value of the inverse.

##This function takes a parameter x, which is an inversible function
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function ALSO takes parameter x (an inversible function) as its main argument, but calls 
## "makeCacheMatrix()" within it. 
## By calling "makeCacheMatrix(x)" within it, cacheSolve() calculates the inverse of the special matrix 
## derived with makeCacheMatrix. However, before performing the calculation, the function first checks
## if the current argument x is also the same x matrix that was used in a previous step. If so, 
## the function prints a message and returns the cached inverse, as opposed to performing the computation again.
## If x has changed from a previous step, then the function calculates the inverse from the beginning.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(exists("lastUsed") && identical(lastUsed,x)==TRUE) {
                        message("getting cached data")
                        return(i) 
        }
        data <- makeCacheMatrix(x)$get()
        i <<- solve(data, ...)
        lastUsed <<- x
        makeCacheMatrix(x)$setinverse(i)
        i
}

