# Caching the inverse of a matrix
# This script contains 2 versions to calculate, cache and check the inverse of a matrix


#This function creates the ability to cache a matrix in order to save time
#It requires a matrix as input, it returns a list which allows to set and get an inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# This function checks whether the inverse of a matrix already has been stored
# If this is the case, then it returns the cached inverse. Else it caches and returns the inverse
# Input for this function is what makeCacheMatrix returns and the matrix that you want to be inversed
cacheSolve <- function(x, mat,...) {
      m <- x$getinverse()
      if(!is.null(m) && x$get()==mat) {
            message("getting cached data")
            return(m)
      }
      data <- mat
      m <- solve(data, ...)
      x$setinverse(m)
      message("inverse was not cached or matrices did not match")
      m
}

