## Creates a special matrix and chaches its inverse


## makes the matrix that contains a list of functions

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## calculates the inverse of the matrix and sets the inverse in the cache.
## If the inverse has already been calculated, then the cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Geting cached inverse")
    return(inv)
  }
  
  mat<-x$get()
  inv<-solve(mat)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}
