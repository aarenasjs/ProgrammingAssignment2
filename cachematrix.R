
#These functions allow to solve the inverse of a matrix and save 
#its value. If the matrix is consulted again, the cached value is returned.


##The makeCacheMatrix function returns a list with a set of functions
##that allows to cached the matrix and access it through setinvma and  getinvma functions

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


#The cacheSolve returns a cached value if available. otherwise 
#calculates its value and caches by setinvma function

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



