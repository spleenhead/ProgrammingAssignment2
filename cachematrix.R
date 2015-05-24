## Assignment 2 - Coursera - R Programming


## Source used to understand assignment example - 
## http://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r
## also r-helpfile ?"<<-" 

## This function creates a special global "matrix" object for storing the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
   
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse from the cache
## Note - it is assumed the matrix is always invertible and the matrix isn't checked

cacheSolve <- function(x, ...) {
  
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    data<- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
