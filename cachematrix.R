# Caching the inverse of a Matrix

# Create a special "matrix", which is realy a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInvMat <- function(invMat) im <<- invMat
  getInvMat <- function() im
  list(set = set, 
       get = get, 
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}

# Calculate the inverse of the special "matrix" if it hasn't been calculated.
# Otherwise, get the inverse from the cache and skip the computation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInvMat()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInvMat(im)
  im
}