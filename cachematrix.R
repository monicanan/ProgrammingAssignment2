## Caching the Inverse of a Matrix to reduce the costly computation
## In order to do so, two functions are used here, one for  storing the matrix, one for cache's its inverse

## The first function,makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
s<-NULL
set <- function(y) {
  x<<-y
  s<<-NULL
}
get<-function() x
setinverse<-function(solve) s<<-solve
getinverse<-function() s
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated(and the matrix has not changed),then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s<-x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix<-x$get()
  s<-solve(matrix,...)
  x$setinverse(s)
  s
  
          ## Return a matrix that is the inverse of 'x'
    }

}
