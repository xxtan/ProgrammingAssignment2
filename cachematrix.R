## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: creates a special "matrix" objects and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set value of the vector 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the vector 
  get <- function() x
  
  ## set the mean into the cache m 
  setmean <- function(mean) m <<- mean
  
  ## return the mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Write a short comment describing this function
## cacheSolve:Compute the inverse of special "matrix" returned by makeCacheMatrix()
## returns inverse from the cache if inverse has already been calculated
## and no changes to the matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmean()
  ## Check if mean had been calculated. If mean is not null, return cache value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get inverse of the matrix   
  data <- solve(x$get())
  ## compute inverse of matrix and setmean  
  m <- mean(data, ...)
  ## set the mean of the inverse
  x$setmean(m)
  m
}