## Here are a pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    getinverse <- function() m
    setinverse <- function(inverse) m <<- inverse
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# test scripts below
# seq1 <- seq(1:4)
# zz<- makeCacheMatrix(matrix(seq1,2))
# zz$get()
# zz$getinverse()

# cacheSolve(zz)
# zz$getinverse()
# cacheSolve(zz)


