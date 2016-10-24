## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as input and creates a "matrix" object, which is a list containing functions 
## to set the value of a matrix, get the value of the matrix, set the value of 
## the inverse of the matrix and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()mat2
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}
