## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y=matrix()){
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setInv <- function(inverse=matrix()) inv <<- inverse
  getInv <- function() inv
  list (set = set, get = get,setInv = setInv, getInv = getInv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
 
  if(!is.null(inv)){
    
    message("getting cached inverse")
    return(inv)
  }
  myMatrix <- x$get()
  inv <-solve(myMatrix,...)
  x$setInv(inv)
  inv
}
