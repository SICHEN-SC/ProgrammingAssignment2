# makeCacheMatrix fucntion ----
## This function creates a "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     
     inv <- NULL
     mat <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     matinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(mat = mat, get = get,
          matinv = matinv,
          getinv = getinv)

}

# cacheSolve function-----
## This function computes the inverse of the matrix returned by `makeCacheMatrix` 

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$matinv(inv)
     inv
}
