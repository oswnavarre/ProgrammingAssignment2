## The functions create a object that stores a matrix and caches 
## the matrix's inverse

## This function creates a special matrix, the function
## does 4 things, get the matrix, set the matrix
## get the inverse, and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the matrix returned by makeCachematrix
## and computes the inverse. If the inverse has been computed, the 
## function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


