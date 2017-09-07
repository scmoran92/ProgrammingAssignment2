## Provides functionality to efficiently compute square matrix inversions
## by avoiding repeated calculations by using caching.

## Takes in a matrix and returns a list of functions to set matrix, get matrix,
#  set inverse, get inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	setInverse <- function(I)
		inverse <<- I

	getInverse <- function()
		return(inverse)

	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function()
		return(x)

	return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## Takes in a matrix and returns the inverse if it's cached, otherwise it computes the 
## inverse and caches it, then returns it.

cacheSolve <- function(x, ...) {
    I <- x$getInverse()
    if (!is.null(I))
    {
    	print("had it cached")
    	return(I)
    }
    print("didnt have it cached, caching")
    data <- x$get()
    I <- solve(data)
    x$setInverse(I)
    return(I)
}