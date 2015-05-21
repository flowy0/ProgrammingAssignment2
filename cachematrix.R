## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv1 <- NULL
    set <- function(y) {
      x <<- y
      inv1 <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv1 <<- inverse
    getinv <- function() inv1
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv2 <- x$getinv()
  if(!is.null(inv2)) { ##check for cache
    message("getting cached data")
    return(inv2)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(inv2)
  inv2
  
}
