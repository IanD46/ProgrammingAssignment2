#The first function, "makeCacheMatrix" creates a special "Matrix", which is
#actually  a list containing a function to:

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverted matrix
#4.  get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
  
}


#This function calculates the inverse of the special "matrix"
#created with the function "makeCacheMatrix". The function has two parts:

#   First, it checks to see if the inverse of the matrix has already been calculated. 
#   If it has, it "gets" the inverted matrix from the cache and skips the computation. 

#   Otherwise, it inverts the new matrix (using the solve function) and "sets" (saves) 
#   the inverted matrix into cache.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
} 



