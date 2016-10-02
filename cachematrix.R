## Programming Assignment 2 for R Programming. 
## makeCacheMatrix: Creates a Special Matrix: List to contain a function 
##      to set/get the value of the matrix, 
##      set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
}


## cacheSolve: Solves the inverse of the matrix using solve(). 
## Unless the inverse has been cached already, then the function fetches the cache
## Assumes the matrix is singular

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
