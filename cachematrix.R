## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()){
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setInverse <- function(Inverse) i <<- Inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(m, ...) {
    m <- m$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    } 
    Data <- m$get()
    i <- solve(Data, ...)
    m$setInverse(i)
    i
}
