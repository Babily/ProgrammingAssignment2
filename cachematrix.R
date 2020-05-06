## This code was written as fullfillment of week 3 assignmnet
## functions create a objected that can be cached and returned without the need for 
## extra computation



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ##define argument with default "matrix"
  inv <<- NULL                              ## inv will hold the value of inverse
  
  set <- function(y){                       ## define set function; if new matrix, inv is NULL
     x <<- y
     inv <<- NULL
  }
  
  get <- function () x                      ## define get function; to return matrix
  setinverse <- function (inverse) inv <<- inverse  ## set inverse matrix
  getinverse <- function () inv                     ## return inverse matrix
  
  list (set = set, get = get,               ## list objects so we can use $ in cachesolve
        setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse ()
    if (!is.null(inv)){
      message("getting cached data")
      inv
          }
    data <- x$get ()
    inv <- solve(data, ...)
    x$setinverse (inv)
    inv
  
}
