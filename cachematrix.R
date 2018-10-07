## Two functions that find and cache the inverse of a matrix

## Creates a cache for a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    setInverse <- function(inv) i <<- inv
    getInverse <- function()i
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Finds the inverse of a matrix. Returns cached inverse if
## inverse has already been found

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
