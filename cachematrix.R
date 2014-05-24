
# The first function, makeCasheMatrix, makes a special matrix where the inverse of this matrix could be stored.
# The second function, casheSolve, returns the inverse of a matrix.



# This function creates a list that contains a list of functions that allows you to: 
      # a) set the value of a matrix (x$set()), 
      # b) get the value of the matrix (x$get()), 
      # c) set the value of the inverse of this matrix (x$setinv()), and
      # d) get the value of this inverse matrix (x$getinv()).
# When creating a matrix using this function, its inverse matrix will be solved and saved. But it could also be set with the x$setinv() function.

makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL
      set <- function(y) {
            x <<- y
            xinv <<- NULL
      }
      get <- function() x
      setinv <- function(inv) xinv <<- inv
      getinv <- function() xinv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}



# This function returns the inverse of a matrix. If the inverse has already been cached, it gets that matrix. If not, it solves the inverse and caches it.

cacheSolve <- function(x, ...) {
      xinv <- x$getinv()
      if(!is.null(xinv)) {
            message("getting cached data")
            return(xinv)
      }
      data <- x$get()
      xinv <- solve(data, ...)
      x$setinv(xinv)
      xinv
}
