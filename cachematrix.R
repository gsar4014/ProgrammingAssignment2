## The functions makeCacheMatrix and cacheSolve take a matrix x, and
## calculate and cache its inverse for easy calling.

## This function takes matrix x as an argument and returns a list object
## containing the function environment, including get and set functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m){
    x <<- m
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the inverse of matrix x by calling the
## functional environment of makeCacheMatrix and calculating the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
