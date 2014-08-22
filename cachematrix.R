## Two functions that are used to cache the inverse of a matrix


## This function creates a cache of the matrix inverse

makeCacheMatrix <- function( m = matrix() ) {
  a <- NULL
  
  ## Set the matrix
  set <- function(matrix) 
  {
    m <<- matrix
    a <<- NULL
  }
  
  ##Get the matrix
  get <- function()
  {
    m
  }
  
  ## Set the inverse of matrix
  setinverse <- function(inv) 
  {
    a <<- inv
  }
  
  ##Get the inverse of matrix
  getinverse <- function()
  {
    a
  }
  
  ## Returns a list containing the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}

## Calculates the inverse of the matrix returned by "makeCacheMatrix method"
## above. If the inverse has already been calculated then the "cachesolve" 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Returns the inverse of x
  m <- x$getinverse()
  
  ## Returns the inverse and a message if it is already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Get the matrix back
  data <- x$get()
  
 ## Matrix multiplication for calculating the inverse
  m <- solve(data) %*% data
  
  x$setinverse(m)
  
 ## Returns the matrix 
 m
  
  
}
