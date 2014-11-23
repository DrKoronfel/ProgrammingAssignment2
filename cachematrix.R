## makeCacheMatrix is a function that makes a list of functions used as an input 
## for cacheSolve function which is used to obtain cached inverse calculations 
## previously performed if it was and performs the inverse calculation if it was not

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  set.Inverse <- function(inverse) m <<- inverse
  get.Inverse <- function() m
  list(set = set, get = get,
       set.Inverse = set.Inverse,
       get.Inverse = get.Inverse)
  
}


## cacheSolve uses input matrix to take inverse
## and checks if the makeCacheMatrix function output has
## inverse calculations in cache. CacheSolve uses the cached solution
## without having to re-calculate the inverse.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  
  
  m <- x$get.Inverse()
  if(!is.null(m)){
    message("getting cached inverse data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$set.Inverse(m)
  m
  
}
