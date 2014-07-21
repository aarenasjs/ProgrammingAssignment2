## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.

makeCacheMatrix <- function(m){
  
  i <- NULL
  
  set <- function(y){
    
    m <<- y
    i <<- NULL
  }
  
  get <- function() m
  setinvma <- function(inv) i <<-inv
  getinvma <- function() i
  list(set=set, get=get,setinvma=setinvma,getinvma=getinvma)
  
}


## Write a short comment describing this function.

cacheSolve <- function(m){
  
  i <- m$getinvma()
  
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  
  data <- m$get()
  i <- solve(data)
  m$setinvma(i)
  i
}

## Return a matrix that is the inverse of 'x'

