## Code creates a matrix object that can cache its inverse.
## Set the input x as the matrix.
## Set the solved value "invrt" as a null.
## Changed "mean" to "solve".

makeCacheMatrix <- function(x = matrix()) {
  invrt <- NULL
  set <- function(z) {
    x <<- z
    invrt <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {invrt <<- inverse}
  getInverse <- function() {invrt}
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## Compute the inverse of the matrix above. 
## If inverse is already calculated then return the inverse matrix.

cacheSolve <- function(x, ...) {
  invrt <- x$getInverse()
  if (!is.null(invrt)) {
    message("Returning cached calculation")
    return(invrt)
  }
  mat <- x$get()
  invrt <- solve(mat, ...)
  x$setInverse(invrt)
  invrt
}
