## <makeCacheMatrix> function outputs a list of 4 sub-functions 
## by using an input of matrix.
## The description of the 4 sub-functions is as followed:
## -- <get_matrix>: outputs the input matrix, which is the same
## ---- as x;
## -- <get_inversion>: outputs the cached inputted matrix's 
## ---- inversion;
## -- <set_matrix>: changes the inputted matrix from outside of
## ---- <makeCacheMatrix> function;
## -- <set_inversion>: changes the cached inversion from outside
## ---- <makeCacheMatrix> function;

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set_matrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function() x
  set_inversion <- function(inversion)  inv <<- inversion
  get_inversion <- function() inv
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inversion = set_inversion, get_inversion = get_inversion)
}


## Using function <makeCacheMatrix>'s output, <cacheSolve> checks
## if there is a cached inversion.
## If there is, it returns the cached value.
## If there isn't, it calculates the inversion of the inputted
## matrix in <makeCacheMatrix>, and then writes the calculation
## to cache.

result_func1 <- makeCacheMatrix(x)
cacheSolve <- function(result_func1, ...) {
  inv <- result_func1$get_inversion()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_data <- result_func1$get_matrix()
  inv <- solve(matrix_data)
  result_func1$set_inversion(inv)
  return(inv)
}
