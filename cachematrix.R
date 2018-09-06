## Based this almost entirely on the provided example, as I'm not very familiar with inverting matrices.

## This function takes in a matrix, then defines several variables as functions intended to be called with cacheSolve.  
## "set" takes in a value y and SuperAssigns x the value of y, then SuperAssigns cached_inversion the value NULL.  
## "get" simply takes in an input.
## "set_inversion" takes in an "inverse" as an argument, then SuperAssigns cached_inversion the value of "inverse", thus
## overwriting the NULL value.
## "get_inversion" takes in an input.
## After these four functions are built, they are stored in a List, to be called upon by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  cached_inversion <- NULL
  set <- function(y) {
    x <<- y
    cached_inversion <<- NULL
  }
  get <- function() x
  set_inversion <- function(inverse) cached_inversion <<- inverse
  get_inversion <- function() cached_inversion
  list(set = set, get = get,
       set_inversion = set_inversion,
       get_inversion = get_inversion)
}


## This function takes in a List of functions ("set", "get", "set_inversion", and "get_inversion").
## First it generates inversion_func and defines it as get_inversion, and checks if that value is NULL.
## If it is, get_inversion has already been run on this value (as it has overwritten the NULL value of cached_inversion
## in makeCacheMatrix), and so the value of inversion_func can simply be returned.
## If not, get() is executed to acquire the Matrix that was passed into makeCacheMatrix, then solve() is run to invert it.
## inversion_func (which was NULL) is now defined as solve(input_matrix), and can be returned to the user.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversion_func <- x$get_inversion()
  if(!is.null(inversion_func)) {
    message("getting cached data")
    return(inversion_func)
  }
  input_matrix <- x$get()
  inversion_func <- solve(input_matrix, ...)
  x$set_inversion(inversion_func)
  inversion_func
}