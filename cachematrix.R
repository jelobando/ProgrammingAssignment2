## The makeCacheMatrix is the function responsible for creating the special matrix object,
## while the cacheSolve computes the inverse after receiving the data from makeCacheMatrix

## makeCacheMatrix returns a list containing few functions in order to fetch the value of set and get before proceeding to processing of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
  
  m <- NULL
  
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse)
  {
    m <<- inverse
  }
  
  getInverse <- function()
  {
    m
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## It returns the inverse of the matrix built with the function above.
## Moreover, this function also retrieves the available cache and computes caches before returning it.

cacheSolve <- function(x)
{
  m <- x$getInverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
