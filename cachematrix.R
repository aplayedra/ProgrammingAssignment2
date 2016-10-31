## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment


makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
  cache <- x$getInverse()
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$get()
  tryCatch ({
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    return(NA)
  },
  finally = {
    x$setMatrix(cache)
  })
  return (cache)
}
