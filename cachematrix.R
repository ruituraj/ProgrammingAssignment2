
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
# set the inverse
        setinv <- function(inv) i <<- inv
# get the inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
        i <- x$getinv()
# check the cache
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
# calculate the inverse
        i <- solve(data)
# set the inverse
        x$setinv(i)
        i
}
