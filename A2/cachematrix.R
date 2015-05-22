## Put comments here that give an overall description of what your
## functions do:
#The makeCacheMatrix function creates a special "matrix",
#which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

#To test this functions we set a new value for any character: 
#a <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) )

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## Write a short comment describing this function:
#The cacheSolve function calculates the inverse of the generated matrix.
#The generated matrix is created with the makeChacheMatrix function.
#The first step of makeCacheMAtrix is to check the values and verify if the 
#inverse was calculated otherwise the inverse calculation is calculated 	
#automatically at the setinverse function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
