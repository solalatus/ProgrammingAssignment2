## This is an implementation of the Coursera "Introduction to R" course assignment 2
## The goal is to make a caching function set for matrix inversion

## This function optionally receives an existing matrix or creates a new one,
## and provides get and set functionalities as a frame for accessing cached data
makeCacheMatrix <- function(x = matrix()) {
	inv <- NaN
  	set <- function(y) {
                x <<- y
                inv <<- NaN
        }
        get <- function() x
        setinverse <- function(sol) inv <<- sol
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This is the main function that serves as a cached calculator of matrix inversion
## It receives a matrix, checks if there is already an inverse cached, if yes, gives back the cached results with an appropriate message,
## if no, calculates the inverse, stores it into the cache and gives it back.
cacheSolve <- function(x, ...) {
        inve <- x$getinverse()
        if(!is.nan(inve)) {
                message("getting cached data")
                return(inve)
        }
        data <- x$get()
        inve <- solve(data, ...)
        x$setinverse(inve)
        inve
}




