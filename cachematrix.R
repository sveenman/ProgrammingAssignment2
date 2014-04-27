## How to use:
## 1. Make a matrix
## x = matrix( c(2, 4, 3, 1, 5, 7, 9, 6, 0, 1, 7, 6, 4, 9, 5, 8), nrow=4)
##
## 2. Pass it to the MakeCacheMatrix function
## cachex <- makeCacheMatrix(x)
##
## 3. Show the matrix
## cachex$get()
##
## 4. Start the cacheSolve 2 times to get the cache inverse
## cacheSolve(cachex)
##
##
## Set and get the matrix and the inverse 
makeCacheMatrix <- function(x = matrix()) {
  ## Need to store the cached matrix
  ic.matrix <- NULL
  
  ## Setting the value
  set <- function(y) {
    x <<- y
    ic.matrix <<- NULL
  }
  
  ## Get the value
  get <- function() x
  
  ## Now set the inverse
  setinver <- function(inverse) ic.matrix <<- inverse
  
  ## Get the inverse
  getinver <- function() ic.matrix
  
  ## New fuctions of the matrix to return
  list(set = set, get = get, setinver = setinver, getinver = getinver)
}

## This cacheSolve will calc the matrix inverse now, but not if it's calculated earlier.
cacheSolve <- function(x, ...) {
  ic.matrix <- x$getinver()
  
  ## It will return if it's calculated before
  if (!is.null(ic.matrix)) {
    message("getting cached data")
    return(ic.matrix)
  }
  
  ## If it's not calculated, execute the solve function...
  data <- x$get()
  ic.matrix <- solve(data, ...)
  
  ## Now the inverse cashing
  x$setinv(ic.matrix)
  
  ## Show
  ic.matrix
}
