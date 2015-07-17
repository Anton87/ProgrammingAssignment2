makeCacheMatrix <- function(mx = matrix(rexp(9, rate=1), ncol=3)) { 
    inv <- NULL
    set <- function(y) {
        mx <<- y
        inv <<- NULL
    }
    get <- function() mx
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

cacheSolve <- function(mx, ...) {
    inv <- mx$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mx$get()
    inv <- solve(data, ...)
    mx$setinverse(inv)
    inv
}