## The two function given below crete a special object that 
##store a matrix and cache its inverse

## This matrix creates a special "Matrix" object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  
  get <- function()x
  
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver 
  list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)

}


## This function computes inverse of special "matrix" object 
##created by the function above.
## If inverse is already caluclated, it should retrieve it from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
