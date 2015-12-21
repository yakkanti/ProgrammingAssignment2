##The function, `makeCacheMatrix` creates a list containing a function to
##1.  set the value of a matrix
##2.  get the value of a matrix
##3.  set the value of the solve
##4.  get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  #initialize inv_matrix to NULL
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  #Getting and Setting cached inv matrix
  get <- function() x
  set_inverse <- function(solve) inv_matrix <<- solve
  get_inverse <- function() inv_matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

##computes the inverse of cacheable matrix returned by makeCacheMatrix function
##if the inv matrix is already available in cache, then cacheSolve retuns the 
##cached inverse matrix
##if the inverse matrix is not in the cache, use solve to compute the inverse matrix and store the value 
##in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'	
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$set_inverse(inv_matrix)
  inv_matrix
}
