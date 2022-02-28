## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function, makeCacheMatrix creates a list containing a function to
## get and set the value of both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #We cache the inverse of the matrix
    inv <- NULL
    
  # Here we set and get the values of the matrix  
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    
  #Here we set and get the values of its inverse  
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    
  #Finally this part returns the list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function


## The function calculates the inverse of the matrix created before.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the result from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets its value in the cache
##  via the setinv function.

cacheSolve <- function(x, ...) {  
    inv <- x$getinv()
    ##   This returns the cached value of the inverse matrix if it is already
    ## calculated
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    ## This gets the matrix
    data <- x$get()
    ## The solve(a, b, ...) function solves the equation a %*% x = b for x, and 
    ## If missing, b is taken to be an identity matrix and solve will return the
    ## inverse of a
    inv <- solve(data, ...)
    ## cache inverse
    x$setinv(inv)
    inv
}
