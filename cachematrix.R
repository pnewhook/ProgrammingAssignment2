# creates a list that can be used to cache matrix solving
# 
# Args:
#   x: an invertable matrix
# Returns:
#   list containing methods to solve and cache a matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolved <- function(solved) s <<- solved
  getSolved <- function() s
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}



# inverts a matrix, with cacheing
# Args:
#   x: a 'matrix' returned by makeCacheMatrix
# Returns:
#   Inverse of matrix x
cacheSolve <- function(x, ...) {
  s <- x$getSolved()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolved(s)
  s
}
