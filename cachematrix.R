## These functions are created to compute
## inversee of a matrix only if it is necessary.
## It inverses that for each matrix, that will be set
## as an argument, functions compute inversee matrix
## only if has not been done yet and save result
## in cache.
## Otherwise function read pre-computed value
## from cahce.

## Set of functions to saving, loading, and computing
## inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that looking for inverse for a given
## input matrix. If there is pre-computed result-
## it is loaded, otherwise it is computed and
## set to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inversee of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
