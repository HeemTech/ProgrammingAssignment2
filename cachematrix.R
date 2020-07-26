## The overall aim of the functions is to retrieve the inverse of a matrix
## from cache. If no value is previously stored, it is to calculate the
## value and store it as cache for retrieving later.
## Step1 : Define a non singular square matrix M.
## step2 : cacheSolve(makeCacheMatrix(M)) to calculate inverse of M

## This function creates a special matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix())
{
  Inv<-NULL
  set<-function(y)
  {
    x<<-y
    Inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) Inv<<-inverse
  getinverse<-function() Inv
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes or retrieves the inverse of the special matrix
##returned by makeCacheMatrix() above

cacheSolve <- function(x, ...)
{
  Inv<-x$getinverse()
  if(!is.null(Inv))
  {
    message('Getting Cache Data ....')
    return(Inv)
  }
  else
  {
    data<-x$get()
    Inv<-solve(data,...)
    x$setinverse(Inv)
    Inv
  }
}