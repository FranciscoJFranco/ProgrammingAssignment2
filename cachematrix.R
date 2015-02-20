## Put comments here that give an overall description of what your
## functions do
#These pair of functions have the goal of caching the inverse of a matrix so that when we call for the inverse of the matrix, if the inverse exists, it does not need to be calculated again.


## Write a short comment describing this function
#makeCacheMatrix creates a list of functions that can store the values of a matrix and its inverse in order to cache it.
makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL  
  set<-function(y){
    x<<-y
    invx<<-NULL
  }
  get<-function() x
  
  setinv<-function(inverse) invx<<-inverse
    
  getinv<-function() invx
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
#cacheSolve receives an object created with makeCacheMatrix and if the inverse of the matrix stored in the object exists already, it returns it. Otherwise it calculates the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx<-x$getinv()
  if(!is.null(invx)){
    message("getting cached inverse")
    return(invx)
  }
  
  mat<-x$get()
  invx<-solve(mat)
  x$setinv(invx)
  invx
  
  
}
