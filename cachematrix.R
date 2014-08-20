## These two functions allow calculation, caching and retrieval 
## of a matrix and its inverse so that the inverse need not be 
## recalculated unecessarily.

## This function creates a special "matrix" that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	## Cache the passed matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## Retrieve the cached matrix
	get <- function() x
	
	## Cache the inverse matrix
	setinverse <- function(inverse) m <<- inverse
	
	## Retrieve the cached inverse matrix
	getinverse <- function() m
	
	## Return a list object with these functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function checks to see if the inverse of the special 
## "matrix" has been cached and, if not, calculates, caches  
## returns it.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	## First check to see if inverse matrix is cached
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## Otherwise, retrieve the cached matrix
	data <- x$get()
	
	## Calculate the inverse matrix
	m <- solve(data, ...)
	
	## Cache the inverse matrix & return it
	x$setinverse(m)
	m
}
