
#Creates the function that will store the matrix in an environment and return 
#directly from memory. This will reduce the computation time
makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      get <- function() {
            x
      }
      setinverse <- function(inverse) {
            inv <<- inverse
      }
      
      getinverse <- function() {
            inv
      }
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#This function finds the inverse of matrix
#If it is already computed earlier it returns the same result
#Else it is computed again
cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()
      
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
