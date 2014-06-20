###
##
###

## The following functions cache the inverse of a square matrix.
##
## @author Dan Murray (dan@murray5.net)
## @version 0.1 06/20/2014

####
## This function creates a special "matrix", which is really a list
## containing functions to:
##
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse matrix
##   4. get the value of the inverse matrix
##
## @param: a matrix
## @return: a List object with setters and getters.
##

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


####
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve retrieves the inverse from the cache.
##
## @param: a matrix
## @return: The matrix inverse
##

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  
  m
}
