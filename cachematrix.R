## The two functions below try to set the matrix data and get their inverse; save it in cache to reduce computation time

## makeCacheMatrix sets the data of matrix to be ready for inverse cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## get inverse if not in cache;

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {  ## check if inverse in is cache
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()  ## get data to calculate inverse
  m <- solve(data, ...)
  x$setInverse(m)
  solve(m)
}
