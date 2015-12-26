## The following functions calculate and cache the inverse of a matrix (nonsingular
## by assumption) that is passed to makeCacheMatrix.

## This function creates a list containing three functions: one to return the
## original matrix (get()), one to set the value of the inverse matrix (setinverse())
##and one to retrieve that inverse (getinverse()).

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list( get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the list outputed from the previous function. If that
## environment already contains an inverse of the original matrix, that inverse
## is returned. Otherwise, the inverse is calculated and cached.

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
