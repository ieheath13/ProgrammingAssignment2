## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      mat_inv <- NULL
      set <- function(y) {
            x <<- y
           mat_inv <<- NULL
      }
      get <- function() x
      setinv <- function (inv) mat_inv <<- inv
      getinv <- function () mat_inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        mat_inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached matrix inversion.")
              return(mat_inv)
        }
        mat_data <- x$get()
        mat_inv <- solve(mat_data, ...)
        x$setinv(mat_inv)
        return(mat_inv)
}
