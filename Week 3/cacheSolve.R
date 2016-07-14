makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function()
                x
        setInverse <- function(inverse) {
                m <<- inverse
        }
        getInverse <- function()
                m
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}
cacheSolve <- function(x, ...) {
        m <- x$getInverse
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}