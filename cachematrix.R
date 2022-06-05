## Two functions that cache the inverse of a matrix (maximizing time-consuming computation).

## The function below creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
          set <- function(y) {
            x <<- y
            invers <<- NULL
          }
          get <- function() x
          setInvers <- function(solveMatrix) invers <<- solveMatrix
          getInvers <- function() invers
          list(set = set, get = get,
               setInvers = setInvers,
               getInvers = getInvers)
}


## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          invers <- x$getInvers()
          if(!is.null(invers)) {
            message("getting cached data")
            return(invers)
          }
          data <- x$get()
          invers <- solve(data)
          x$setInvers(invers)
          invers
}
