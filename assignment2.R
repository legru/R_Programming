## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  # store for the inverse matrix
  inverse <- NULL
  # setting/getting functions
  set <- function(data) {
    matrix <<- data
    inverse <<- NULL
  }
  get <- function() matrix
  set_inverse <- function(matrix.inverse) inverse <<- matrix.inverse
  get_inverse <- function() inverse
  # return list of setters and getters
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## function to construct the matrix inverse
# if the inverse has been cache then return it
# otherwise generate it.
cacheSolve <- function(x, ...) {
  x.inverse <- x$get_inverse()
  if(!is.null(x.inverse)) {
    message("getting cached data")
    return(x.inverse)
  }
  # compute inverse of x
  x.matrix <- x$get()
  x.inverse <- solve(x.matrix)
  x$set_inverse(x.inverse)
  ## Return a matrix that is the inverse of 'x'
  x.inverse
}