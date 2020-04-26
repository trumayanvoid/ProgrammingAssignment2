## This contains two functions, makeCacheMatrix() and cacheSolve(). 
## makeCacheMatrix() creates an R object storing 
## a vector and its matrix inverse.
## cacheSolve() uses an argument returned by makeCacheMatrix
## to retrieve the matrix inverse from the cached value
## stored in the makeCacheMatrix() object's environment.


## Creates an R object storing a vector and its matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function (y) 
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## if inverse is already calculated, this retrieves the inverse from cache


cacheSolve <- function(x, ...) 
{
  i <- x$getinverse()
  if(!is.null(i)) 
  {
    message ("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}  


