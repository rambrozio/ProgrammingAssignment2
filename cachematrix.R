## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that will cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mtx_inv <- NULL
  
  set <- function(y) {
    x <<- y
    mtx_inv <<- NULL
  }
  
  get <- function() x
  setmtx_inv <- function(solve) mtx_inv <<- solve
  
  getmtx_inv <- function() mtx_inv
  
  list(set = set, get = get,
       setmtx_inv = setmtx_inv,
       getmtx_inv = getmtx_inv)
}



## Write a short comment describing this function

        ## Return a matrix that is the inverse of 'x'
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mtx_inv <- x$getmtx_inv()
  if(!is.null(mtx_inv)) {
    message("getting cached data")
    return(mtx_inv)
  }
  data <- x$get()
  mtx_inv <- solve(data, ...)
  x$setmtx_inv(mtx_inv)
  mtx_inv
}
