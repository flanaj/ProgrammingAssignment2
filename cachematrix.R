## Put comments here that give an overall description of what your
## functions do

## crate a list of four functions that are getters and setters for a 
## matrix x and its inverse. throw error if matrix is not square

makeCacheMatrix <- function(x = matrix()) 
{
      inverse <- NULL
      if (ncol(x) != nrow(x))
      {
            stop("matrix must be square")
      }
      set <- function(y) 
      {
            if (ncol(y) != nrow(y))
            {
                  stop("matrix must be square")
            }
            x <<- y
            inverse <<- NULL
            
      }
      get <- function() x
      setinverse <- function(i) inverse <<- i
      getinverse <- function() inverse
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## if the inverse is null cache it and then return it

cacheSolve <- function(x, ...) 
{
      if (is.null(x$getinverse()))
      {
            message("caching inverse")
            x$setinverse(solve(x$get(), diag(ncol(x$get())), ...))
      }
      x$getinverse()
}
