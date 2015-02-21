## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##

#This group of functions will calculate the inverse of a matrix (using the
#solve function) and will cache the result so that it can be accessed
#without having to re-perform the calculation. To do this, the we first check
#to see if the inverse has already been calculated, and if so, we just
#return the cached values.  If the inverse hasn't been calculated, we
#calculate it and cache it.

#The first function, makeCaheMatrix, will create a list that has four functions
#in it.  These are set, which sets the values of the matrix;
#get which gets and returns the matrix; setinverse which calculates and caches
#the inverted matrix; and getinverse which gets and returns the inverted matrix
#from the cache.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <-- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##
#The second function, cacheSolve, first checks to see if the inverted matrix has aleady
#been cached, and if so it returns it.  If it hasn't already been cache, then the function
#will get the matrix, compute its inverse using solve, and will cache the result.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
