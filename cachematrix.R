## Matrix inversion is usually a costly computation 
## There may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## There are also alternatives to matrix inversion that are not covered here
## This function cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inverse <<- i
	getinverse <- function() inverse
	list(set = set, get = get, 
	setinverse = setinverse, 
	getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, we assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
      if(!is.null(inverse)) {
        	message("getting cached inverse")
        	return(inverse)	
	}
	data <- x$get()
	inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
# data <- matrix(c(1, 1, 4, 0, 3, 1, 4, 4, 0), nrow=3, ncol=3)
# solve(data)
#     	       [,1]        [,2]    [,3]
#	[1,]  0.08333333 -0.08333333  0.2500
#	[2,] -0.33333333  0.33333333  0.0000
#	[3,]  0.22916667  0.02083333 -0.0625
