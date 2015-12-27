## These 2 functions will calculate and return the inverse of a given matrix.

## The first function caches the Matrix which will be used for inverse calculation. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixinv <- function(matrix) m <<- matrix
        getmatrixinv <- function() m
        list(set = set, get = get,
             setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv)
}

## The second function:
## If the inverse of the matrix is not already calculated
## (e.g. if this is the first time function runs), 
## then it calculates the inverse of the matrix.
## If it exists then the value is returned from the cache.

cacheSolve <- function(x, ...) {
		## makeCacheMatrix is assingned to x.
        ## assign cached inverse value of matrix to m
		m <- x$getmatrixinv()
		## check if the value exists. if yes, return and exit, else go on.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		## get data from x(makeCacheMatrix).
        data <- x$get()
		## calculate the inverse and assign it to m.
        m <- solve(data, ...)
		## cache the inverse of the matrix.
        x$setmatrixinv(m)
		## return inverse of the matrix.
        m
}
