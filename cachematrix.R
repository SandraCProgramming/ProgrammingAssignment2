## makeCacheMatrix creates a list containing functions to:
## 1. set matrix
## 2. get matrix
## 3. set inverse of matrix
## 4. get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve is a function that computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.
## If inverse is already calculated then cachesolve should 
## retrieve inverse from cache.
	
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
}