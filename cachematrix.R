## Functions to cache the inverse of the matrix rather than 
## computing it repeatedly.

## makeCacheMatrix creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(b){  ##set value of the matrix
    x<<-b
    i<<-NULL
  }
  get<-function() x  ##get the value of matrix
  setinverse<-function(solve) i<<- solve ## set inverse of matrix
  getinverse<-function()i     ##get inverse of matrix     
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special
## "matrix" returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  
  ## If inverse is already calculated, retrive the inverse from cache and return it.
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ##else return a matrix that is the inverse of 'x'
  ma<-x$get()
  i<-solve(ma)
  x$setinverse(i)
  i
}
