## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1.set the value of the matrix 2.get the value of the matrix 
##3.set the value of the inverse 
##4. get the value o the inv?

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function()x
  setInverse <- function() m <<- solve(x, ...)
  getInverse <- function() m
  
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
    
  }
  #if nothing has been cached - calculate the inverse 
  mdata <- x$get()
  m <- solve(mdata, ...)
  x$setInverse(m)
  m
              
}
