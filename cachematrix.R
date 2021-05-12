## Overall Description:
##    This pair of function can be used to compute efficiently the inverse of a 
## square, invertible matrix. Since matrix inversion is usually a costly 
## computation, there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. 
##    These functions offer a solution by caching the inverse of a matrix, and
## retrieve the result rather than recompute it when needed. We use the <<- 
## operator to store each matrix and its inverse in the same environment.
##
## Example usage:
##  mxObj <- makeCacheMatrix( runif(1:9) ) # Create matrix with Cache functions 
##  cacheSolve( mxObj ) # 1st pass: NOT using cached data, but Solve & Cache
##  cacheSolve( mxObj ) # 2nd (etc.) pass: getting and giving cached data,
##                      # hence saving computation time
##
## WARNING: Nesting the 2 functions will not make use of the cache,because the
## temporary variable & scope will be lost every time:
##  cacheSolve( makeCacheMatrix( randmx ) ) # called successively, will never
##                                          # make use of the data cache


## Bind a list of functions to environment of given matrix data. The returned 
## functions store data in a scope specific to the given matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Define inverted matrix so it's in the same environment as functions below 
  mxInverted <- NULL
  
  # Set both matrix data, and store them in the same environment   
  set <- function(y) {
    x <<- y
    mxInverted <<- NULL
  }
  
  # Return original matrix data 
  get <- function() x
  
  # Calculate the inverted matrix, and store it in this environment 
  setInv <- function(mxSolved) mxInverted <<- mxSolved
  
  # Return the inverted matrix, consistent with the environment of original
  getInv <- function() mxInverted
  
  # Return the list of functions, bound to specific matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Calculate the inverse of input matrix if NOT previously calculated, cache it
## and return it.
## Otherwise return the already computed, cached result (hence saving time).
cacheSolve <- function(x, ...) {
  # Get inverted matrix
  mxInverted <- x$getInv() 
  
  # If not empty, then return it
  if(!is.null(mxInverted)) {  
    message("cacheSolve(): getting cached data")
    return(mxInverted)
  }
  data <- x$get() # Set original matrix data
  mxInverted <- solve(data, ...) # Compute inverse matrix
  x$setInv(mxInverted) # Cache inverse matrix
  mxInverted # .. and return it
}

