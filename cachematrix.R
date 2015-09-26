## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse

## In particular makeCacheMatrix contains 4 functions: get, set, getinverse, setinverse
## 1. set() - set the value of the matrix in the variable x and empty the inverse matrix
## 2. get() - get the value of the matrix
## 3. setinverse() - set the inverse of the matrix in the variable "inverse"
## 4. getinverse() - get the inverse matrix of the matrix passed in input

## Then it stores the 4 functions in a list, so than we can also assign the
## makeCacheMatrix function to an object and use them singularly
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated and the
## matrix has not been changed, then the cacheSolve retrieves the inverse 
## from the cache. Otherwise it recomputes the inverse matrix and returns the new one
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
