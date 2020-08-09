## the following function will first create an inversible matrix, and then return the matrix inversed.

## the makeCacheMatrix function create a matrix that is inversible

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## the cacheSolve function first checked if the above function contain the inversed matrix, if yes then retrive the matrix, if 
##not then calculate and return the matrix inversed 

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix = x$get()
  inv = solve(matrix, ...)
  x$setinv(inv)
  return(inv)
}
