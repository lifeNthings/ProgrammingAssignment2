## Assignemnt 2 for R Programming course:
## Two functions for calculating and cahching the inverse 
## of a matrix

## Creates a special matrix that caches the inverse of the 
## matrix as long as it stays unchanged.

makeCacheMatrix <- function(x =  matrix()) {
  ## Returns a new matrix object that can cache it's inverse
  i <- NULL
  
  # Functions for changing and returning the stored matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  # Functions for calculating and storing the inverted matrix
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  # The new matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates and returns the inverse of a special matrix,
## or returns a cached version if one has been calculated 
## for the same matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    # If a cached inverse exists, return it
    if(!is.null(i)){
         message("getting cached data")
         return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}

