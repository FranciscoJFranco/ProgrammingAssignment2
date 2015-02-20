## Put comments here that give an overall description of what your
## functions do
#These pair of functions have the goal of caching the inverse of a matrix so that when we call for the inverse of the matrix, if the inverse exists, it does not need to be calculated again.


## Write a short comment describing this function
#makeCacheMatrix creates a list of functions that can store the values of a matrix and its inverse in order to cache it.
makeCacheMatrix <- function(x = matrix()) {
  #Declare and define inverse variable
  invx<-NULL  
  
  #Function to set value of matrix
  set<-function(y){
    #Assign in parent environment
    x<<-y
    invx<<-NULL
  }
  
  #Function to get value of matrix
  get<-function() x
  
  #Function to set inverse of the matrix in the parent environment
  setinv<-function(inverse) invx<<-inverse
  
  #Retrieve inverse of the function
  getinv<-function() invx
  
  #Return the list of the functions declared
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
#cacheSolve receives an object created with makeCacheMatrix and if the inverse of the matrix stored in the object exists already, it returns it. Otherwise it calculates the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Get the inverse of the special matrix
  invx<-x$getinv()
  
  #If the inverse exist, print message and return cached inverse
  if(!is.null(invx)){
    message("getting cached inverse")
    return(invx)
  }
  #If it does not exist, retrieve raw matrix object
  mat<-x$get()
  
  #Calculate inverse
  invx<-solve(mat)
  #Cache inverse
  x$setinv(invx)
  #Return inverse
  invx
}
