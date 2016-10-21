## In order to save on memory and computation time it is smart to store values that
## need to be used often in a system cache. THis allows the system to pull the value from 
## the cache instead of computing the value every iteration of a loop.

## The makeCache Matrix function returns a list of functions to create a special
## cached matrix. The set function initializes the data, the get function finds the data, setinv sets
## the function that we want to run on the data into a cache, and getinv function check the cache for the value .

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m <<-solve
  getinv<-function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The cacheSolve function takes the list of functions we created with the make CacheMatrix and returns the inverse
## of the initial matrix. First it checks if the inv is in the cache and if not uses the get and setinv functions to 
## add the inv to the cache, as well as returning the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinv(m)
  m
}
