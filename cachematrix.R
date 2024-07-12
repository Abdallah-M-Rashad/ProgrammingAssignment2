## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(new_mat) {mat <<- new_mat;inv <<- NULL}
        get <- function() mat
        setinvers <- function(invers) inv <<- invers
        getinvers <- function() inv
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- mat$getinvers()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        new_mat = mat$get()
        inv = solve(new_mat,...)
        mat$setinvers(inv)
        inv
}
