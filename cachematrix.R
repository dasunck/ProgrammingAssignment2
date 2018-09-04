## R Programming - Programming Assignment 2: Lexical Scoping

## Creating a special matrix object that stores a matrix and it's cached mean.
makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) xInverse <<- inverse
        getInverse <- function() xInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Retrieve the matrix inverse value from cache if matrix inverse is already available in cache.
## Otherwise calculate the matrix inverse,store in cache and  return the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		xInverse <- x$getInverse()
        if(!is.null(xInverse)) {
                message("getting cached data")
                return(xInverse)
        }
        data <- x$get()
		##Computing the inverse of a square matrix using solve
        xInverse <- solve(data, ...)
        x$setInverse(xInverse)
        xInverse
}
