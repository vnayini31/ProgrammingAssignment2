## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix returns a list containing three functions as output while taking a matrix as an input.
#cachesolve returns an inverse of an input matrix through solve() if the inverse is already not stored on the cache, if the inverse
#is already in the cache then the value is simply retrieved using get_inverse

## Write a short comment describing this function
#makeCacheMatrix returns a list containing three functions as output while taking a matrix as an input.
#get() returns the input
#set_inverse() stores the inverse of the input matrix in a variable called 'm'
#get_inverse() retrieves the inverse of the matrix already stored on the cache


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set=set,get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## Write a short comment describing this function
#cacheSolve returns the inverse of a matrix that has first been called by the makecachematrix function
#if the called matrix already has been cached then the inverse is simply retrieved from the cache using get_inverse,else it is calculated and
 # stored in cache through set_inverse and then output.


cacheSolve <- function(x, ...) {
        
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set_inverse(m)
  m
  
}
