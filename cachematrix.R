## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The following
## pair of functions cache the inverse of a matrix so it is computed only once and can be
## access many times without recomputing it.


## This function creates a special "matrix" object that can cache its inverse. The object is just a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matirx
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setMatixInverse <- function(inverse) matrixInverse <<- inverse
  getMatixInverse <- function() matrixInverse
  list(set = set, get = get,
       setMatixInverse = setMatixInverse,
       getMatixInverse = getMatixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## It is assumed that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        
  matrixInverse <- x$getMatixInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setMatixInverse(matrixInverse)
  matrixInverse
}
