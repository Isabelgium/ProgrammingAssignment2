## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) { #set the matrix
    x <<- y  #assign a value to an object in an environment that is different from the current environment.
    inv <<- NULL
  }
  getmatrix <- function() x # get the matrix
  
  #inverse of matrix (set and get)
  setinverse <- function(ginv) inv <<- ginv #Install MASS package for using the inverse function ginv
  getinverse <- function() inv
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <-  x$getinverse() #get invers matrix
  if(!is.null(inv)) { #if inverse matrix already in cache
    message("getting cached data") 
    return(inv)
  }
  data <- x$getmatrix() #get matrix
  inv <- ginv(data, ...)
  x$setinverse(inv)
  inv
}
