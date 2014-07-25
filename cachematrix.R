## Creates an object that contains functions for getting and setting a matrix and
## its inverse value.  These functions are contained within a list variable which is also the return object
## Optionally takes a matrix parameter x. 


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## Stores matrix and initializes inverse property
	set <- function(val) {
		x <<- val
		inv <<- NULL
	
	}
	
	## Stores inverse value for this matrix object
	setInverse <- function(inverse) inv <<- inverse
	
	## Retrieves the actual matrix value
	get <- function() x

	## Retrieves the inverse matrix of the matrix value
	getInverse <- function() inv
	
	## Return the list exposes all get/set functions of this object
	list(set=set, setInverse=setInverse, get=get, getInverse=getInverse)
}


## Takes a makeCacheMatrix object as input.
## If the makeCacheMatrix has an inverse already calculated then it returns that value
## If no inverse is available then it is calculated, stored within the makeCacheMatrix, and used 
## and the return value

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()

	## If inverse already cached then return cached value
	if(!is.null(inv)){

		inv
	}
	
	## calculate inverse
	inv <- solve(x$get())
	
	## cache inverse in matrix object
	x$setInverse(inv)	

	inv
}
