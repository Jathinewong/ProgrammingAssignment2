## Date: June 18 2014
## makeCacheMatrix: This function cache a matrix and its inverse
## cacheSolve: This function compute the inverse of a matrix returned by the makeCacheMatrix function.
## If the inverse has already been calculated, return the inverse from the cache.

## makeCacheMatrix: This function cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y 
				inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: 
## This function compute the inverse of a matrix returned by the makeCacheMatrix function
## or return the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
		if ( !is.null(inv) ) {
				message("getting cached inverse")
				return(inv)
		}
		data <- x$get()
		inv <- solve(data)
		x$setinverse(inv)
		inv
}
