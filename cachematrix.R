## Caching the inverse of a matrix. If the inverse of matrix has been calculated,
## the cached result will be returned. Otherwise, the inverse will be calculated
## and the result will be cached.

### makeCacheMatrix is a list of function to
### set the value of the matrix,
### get the value of the matrix,
### set the value of the inverse matrix,
### get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y){
    x<<-y
    invx <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) invx<<-inverse
  getInv <- function() invx
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve calculates the inverse of the matrix created with the 
## makeCacheMatrix. It first check if the inverse has been calculated.
## if yes, it will retrive the inverse from cached value. Otherwise
## it will calculated the inverse using R:solve and cached the result 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getInv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setInv(invx)
  invx
}
