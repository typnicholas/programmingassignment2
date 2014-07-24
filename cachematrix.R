# The first function (makeCacheMatrix) creates a special "matrix", which is 
# really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the value of the inverse matrix
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse matrix
  set_inverse <- function(inv_input) inv <<- inv_input
  # get the value of the inverse matrix
  get_inverse <- function() inv
  # return a list of the above functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# generates the matrix using makeCacheMatrix
x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# returns the matrix
x$get()

# The second function (cacheSolve) calculates the inverse of the special
# "matrix" created with the first function. However, It first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the 
# matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  # check if the inverse matrix is already cached, if so, we get the inverse 
  # matrix from the cache directly
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # else, we first get the matrix
  data <- x$get()
  # then calculate the inverse matrix
  inv <- solve(data, ...)
  # then cache the inverse matrix
  x$set_inverse(inv)
  # finally, return the result
  inv
}

# generates the inverse matrix using cacheSolve
cacheSolve(x)
# returns the inverse matrix
x$get_inverse()
