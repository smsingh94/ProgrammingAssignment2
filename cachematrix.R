## Two functions that cache the inverse of a matrix

## function to cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	set <- function(y) {
		x <<- y
		n <<- NULL
	}
	get <- function() x
	setInverse <- function() n <<- solve(x)
	getInverse <- function() n
	list(set = set, get = get, 
			setInverse = setInverse,
			getInverse = getInverse)	
}


## function to return matrix that is the inverse of x

cacheSolve <- function(x = matrix(), ...) {
	n <- x$getInverse()
	if(!is.null(n)){
			message("getting cached data")
			return(n)
	}
	matrix <- x$get()
	n <- solve(matrix, ...)
	x$setInverse(n)
	n				
       
}

