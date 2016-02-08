## These function cache a matrix in the global environment so that they can be retrieved at a later point without using so much of the cpu.

## This function creates a vector in which to cache the new matrix inversion

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


## this function takes the matrix and inverts it, then stores it in the previous vector.

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
