## This function creates a special "matrix" object that can cache its inverse.
## It includes functions to set and get the matrix, as well as to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(new_mat) {mat <<- new_mat; inv <<- NULL}
        get <- function() mat
        setinvers <- function(invers) inv <<- invers
        getinvers <- function() inv
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinvers()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        new_mat <- x$get()
        inv <- solve(new_mat, ...)
        x$setinvers(inv)
        inv
}