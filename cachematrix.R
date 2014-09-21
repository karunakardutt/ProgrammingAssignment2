## Obtaining the inverse of a matrix using solve() is a time-consuming operation. 
## The functions makeCacheMatrix and cacheSolve work together to cache (store) the inverse, 
## the very first time a matrix is "solved".
## A repeat call to cacheSolve to obtain the inverse of the same matrix (especially useful in a loop) 
## returns the cached result instantly.
## Using these 2 functions avoids repetitive calculations,without compromising on accuracy.
## Typical Usage :
## > m <- matrix(c(0,2,1,0), nrow = 2, ncol = 2, byrow = TRUE)
## > m
##      [,1] [,2]
## [1,]    0    2
## [2,]    1    0
## > mc <- makeCacheMatrix(m)
## 
## > cacheSolve(mc)
##      [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
##
## > cacheSolve(mc)
## Getting cached data ..
##      [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
## > 
##


## makeCacheMatrix takes a matrix as an argument and returns a matrix endowed with a list of 4 functions
## These functions can "set" (store) values in an environment that is not the calling environment, 
## and "get" (retrieve) from the environment where the values were stored using the << operator.
## As used here:
## set -- Stores the matrix, nulls any stored inverse 
## setinverse -- Stores the inverse of a matrix (Note: it does not compute the inverse; just stores it!)
## get -- Retrieves the matrix from the environment where it was stored
## getinverse -- Retrieves the inverse of the matrix from the environment where it was stored. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  
  set <- function (y){
        ## Stores y in x  (keeps in a different environment from the calling one)
        ## Stores Null in the inv (keeps in a different environment from the calling one)
      x   <<- y  
      inv <<- NULL 
  }
  
  get <- function() x    ## Returns x .. current value 
  
  setinverse <- function(storethisinv) inv <<- storethisinv ## Store in a different environment from the calling one
  
  getinverse <- function() inv  ## Returns inv .. current value
  
  ## Return a list of functions with the object
  list (set = set, 
        get=get, 
        setinverse=setinverse, 
        getinverse=getinverse)  
}


## cacheSolve is used in place of the solve() function.
## Because the matrix was built with makeCacheMatrix) it has set(),get(),setinverse() and getinverse() functions.
## The first time cacheSolve is called it finds that the stored inverse is null; it solves the matrix (the first and only time) 
## and caches the inverse by using setinverse(). 
## On the next call cacheSolve checks if the inverse is already stored with getinverse() and returns the "cached" result .


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()  ## Get the stored value of inverse

    if (!is.null(i)){
        message("Getting cached data ..")
        return(i)
    }
    ## We do not have cached inverse we have to compute it    
    mymatrix <- x$get()     ## Get the matrix we need to work on    

    i <- solve(mymatrix, ...)  ## Solve it
    x$setinverse(i)  ## Store the inverse for future usage
    
    ## Return the inverse that is computed   
    i        
}
