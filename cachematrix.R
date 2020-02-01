## These two functions interact each other to use the inverse of a matrix stored
## in the cache if the matrix remains the same.  If tha matrix changes of the 
## inverse is not stored, then cacheSolve function computes the inverse and 
## store it in the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function verifies if there is an inverse of matix "x" in the cache.
## If not, it computes the inverse and store it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m  
  
}
