## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##the function makeCacheMatrix creates a matrix object that cache its inverse.
makeCacheMatrix<-function(x= matrix()){
  inv<-NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
get<-function()x
setinverse<-function(inverse) inv<<-inverse
getinverse<-function(inv)
list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}
## Write a short comment describing this function
##The cacheSolve function returns the inverse of the matrix computed by 
##the function makeCacheMatrix above
cacheSolve <- function(x, ...){
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## Return a matrix that is the inverse of 'x'
  data<-x$get()
  inv<-solve(data)
  x$setiverse(inv)
  inv
}