## The pair of functions below cache the inverse of a matrix,
## reducing the likelihood of time-consuming computations
 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setmatrix <- function(mtx) matrix <<- mtx
  getmatrix <- function() matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##This function computes the inverse of the special "matrix" 
  ##returned by makeCacheMatrix  above
##If the inverse has already been calculated 
  ##then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  matrix <- x$getmatrix()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)  #matrix invert
  x$setmatrix(matrix)
  matrix
}
