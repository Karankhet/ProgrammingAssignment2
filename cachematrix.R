## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat_sol <<- NULL
  setmat <- function(y){
    x <<- y
    mat_sol <<- NULL
  }
  getmat <- function() x
  setinverse <- function(sol) mat_sol <<- sol
  getinverse <- function() mat_sol
  list(getinverse = getinverse, 
       setmat = setmat,
       getmat = getmat,
       setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  sol <- x$getinverse()
  if (!is.null(sol)){
    message("getting cache data...")
    return (sol)
  }
  else{
    data <- x$getmat()
    mat_sol <- solve(data, ...)
    x$setinverse(mat_sol)
    mat_sol
  }
}
