## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( m = matrix() ) {
  a <- NULL
  
  set <- function(matrix) 
  {
    m <<- matrix
    a <<- NULL
  }
  
  get <- function()
  {
    m
  }
  
  setinverse <- function(inv) 
  {
    a <<- inv
  }
  
  getinverse <- function()
  {
    a
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setinverse(m)
  
  m
  
  
}

