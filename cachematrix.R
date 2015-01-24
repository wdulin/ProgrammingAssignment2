## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Returns a vector of functions used to facilitate
## the caching of matrix inverses. 
##  set - stores a matrix and clears any previously cached 
##        inverses
##  get - returns the stored matrix
##  setinv - sets the inverse of the stored matrix
##  getinv - returns the inverse of the stored matrix
##        or NULL if one hasn't been calculated yet.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(iv) inv <<- iv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}



## Write a short comment describing this function
##
## Returns the inverse of a invertable matrix.
## It takes an vector argument of functions created
## by makeCacheMatrix. If the inverse of the matrix has 
## already been calculated then it returns the stored 
## value otherswise it performs the inversion, returns
## it and uses the passed function vector to store it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting caced data")
    return(inv)
  }
  data <- x$get
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
