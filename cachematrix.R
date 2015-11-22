## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object that can cache its reverse.

makeCacheMatrix <- function(x = matrix()) {
## x is a square invertible matrix, used as input for makeCacheMatrix function
inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## Output: is a list of 4 functions that will be used as inputs in cacheSolve
##				1. Set the matrix // lines 8 - 11
##				2. Get the matrix // line 12
##				3. Set the inverse of the matrix // line 13
##				4. Get the inverse of the matrix // line 14

## CacheSolve computes the inverse of the output of makeCacheMatrix function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## x: output of makeCacheMatrix()
  
  inv = x$getinv()
  
  ## check if the inverse has already been calculated
  if (!is.null(inv)){
  ## If yes, get from the cache and skip other actions
    message("getting cached data")
    return(inv)
  }
  ## If not (else), calculates the inverse now:

  mt.data = x$get()
  inv = solve(mt.data, ...)
  
  ## After calculated, sets the value of the inverse in the cache

  x$setinv(inv)
  
  return(inv)

}
