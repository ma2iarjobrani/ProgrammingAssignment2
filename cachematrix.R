
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  set <- function(y){
    x <<- y
    result <<- NULL
  }
  get <- function() x
  setInv <- function(y) result <<- y
  getInv <- function() result
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
    message("Getting cached inverse ...")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
