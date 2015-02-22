#
# Functions to access matrix and its inverse
#
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    if (!is.null(x) && x != y) {
      x <<- y
      invMatrix <<- NULL      
    }
  }
  get <- function() x
  setinv <- function(inv) invMatrix <<- inv
  getinv <- function() invMatrix
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv
  )
}

# 
# Function that retrieve cached inverse matrix if it was computed or
# compute inverse matrix and store it otherwise.
# 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
