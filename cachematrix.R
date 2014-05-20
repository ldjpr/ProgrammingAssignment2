## The following functions are used to compute the inverse matrix of an input matrix and cache its result.
## If the inverse matrix of the input matrix has already been computed, a cached result is returned.
## Otherwise, the inverse matrix is computed.
## Test, create matrix: mat <- matrix(c(51, 60, 7, 8), nrow=2, ncol=2)
## 


## makeCacheMatrix: Creates a a special object "matrix" that stores a matrix, computes its inverse using the solve function 
##  and caches the result.
##Test, amekCacheMatrix: a<-makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## cacheSolve: Calculates the inverse matrix using the solve function with the matrix object
## created in the makeCacheMatrix function.
##Test, get cachec value or compute: cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
