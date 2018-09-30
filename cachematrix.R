setwd("C:/Users/Darshu/Assign2/ProgrammingAssignment2")
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y  # This y is in the function nothing is sent to y hence it does not return anything
            inv <<- NULL # Inv is set as a gloabal variable matrix
      }
      get <- function() x # get function also returns argument does nothing
      setinv <- function(solve) inv <<- solve # Setmean function also returns argument does nothing
      getinv <- function() inv # nothing is sent also returns argument does nothing
      list(set = set, get = get, 
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv <- x$getinv()# check this
      if(det(x$get()) == 0 ) {
            message("Determinant is Zero or input matrix is NULL-- so inverse does not exisit")
            return(x)
      }
      else if (!is.null(inv)) {
            message('Getting Cached Inverse matrix')
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ... = )
      x$setinv(inv)
      inv
}
