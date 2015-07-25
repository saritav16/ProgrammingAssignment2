## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function




##caches the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  cacheinverse <- NULL
  set <- function(y) {
    x <<- y
    cacheinverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cacheinverse <<- inverse
  getInverse <- function() cacheinverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cacheinverse <- x$getInverse()
  
  
  if (!is.null(cacheinverse)) {
    message("getting cached data")
    
    return(cacheinverse)
  }
  
  matrix <- x$get()
  
  
  tryCatch( {
    # set and return inverse of matrix
    cacheinverse <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    
    x$setInverse(cacheinverse)
  } )
  
  
  return (cacheinverse)
}
