makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() {
    x
  }

  setinverse <- function(inverse) {
    inv <<- inverse
  }

  getinverse <- function() {
    inv
  }

  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)

  inv
}

source("cachematrix.R")

x <- matrix(c(1, 2, 3, 4), 2, 2)

m <- makeCacheMatrix(x)

# First call: calculate inverse
cacheSolve(m)

# Second call: retrieve cached inverse
cacheSolve(m)

# Verify correctness
all.equal(cacheSolve(m), solve(x))

# Change matrix and invalidate cache
m$set(matrix(c(2, 0, 0, 4), 2, 2))

# Calculate inverse of new matrix
cacheSolve(m)
