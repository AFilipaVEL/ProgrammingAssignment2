
# Function "makeCacheMatrix" creates a special "matrix", which is really a list containing a function to

#    set the value of the matrix
#    get the value of the matrix
#    set the value of the correspondent inverse matrix
#    get the value of the correspondent inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Function "cacheSolve" calculates the inverse matrix of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse
# matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and
# sets the inverse matrix in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
