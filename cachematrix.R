## These pair of functions help create an augmented matrix object that stores a cached value for its inverse.

## This function takes a given matrix as input, and creates an object that contains it 
## as well as a set of methods that help in getting and setting its values, as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv_value) inv <<- inv_value
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This functions takes a given makeCacheMatrix object and calculates and sets its inverse
## if it founds the inverse has already been calculated, just returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
