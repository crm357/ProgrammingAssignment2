## we begin with a set of 4 functions that both set/get a matrix
## and also to set/get the matrix inverse.

# 1.  set the matrix
# 2.  get the matrix
# 3.  set the inverse of the matrix
# 4.  get the inverse of the matrix

# We first make the function that sets a matrix and associated functions
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## We then make a function that retrieves the matrix inverse
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("returning cached inverse")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
