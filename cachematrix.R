# R Programming -  Programming Assignment 2 - Caching the Inverse of a Matrix

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())  {
	inv <- NULL

	# Set the given matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	# Get the matrix
	get <- function() x

	# Set the inverse matrix
	setinverse <- function(inverse) inv <<- inverse

	# Get the inverse matrix
	getinverse <- function() inv

	# Return the list of functions
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	# Get the inverse matrix
	inv <- x$getinverse()

	# Check if this inverse matrix is from the cache and return it
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
      }
      
      # If not, calculate the inverse matrix 
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv

}

