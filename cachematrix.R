## Caching the inverse of a Matrix

## makeCacheMatrix is a function that creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
  {
    if(!identical(x,y))
    {
       m<<-NULL    
       x<<-y
    }

  }
  get<-function()x
  setsolve<-function(solve) m<<-solve
  getsolve<-function()m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve is a function that computes the inverse of the special matrix
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
