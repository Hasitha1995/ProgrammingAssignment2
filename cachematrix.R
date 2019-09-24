## With these two functions we trying to optimize matrix inversion by caching already calculated
## matrix inverses. The idea is to not to calculate the invese of the same matrix over and over 
## again 

## This function returns a list of functions that is requred to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        Inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                Inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(Inverse) Inverse_matrix <<- Inverse
        getinverse <- function() Inverse_matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks whether there exists a chached inverse for a particular matrix and
## only if it does not exist, the function will calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse_matrix <- x$getinverse()
        if(!is.null(Inverse_matrix)) {
                message("getting cached data")
                return(Inverse_matrix)
        }
        data <- x$get()
        Inverse_matrix <- solve(data, ...)
        x$setinverse(Inverse_matrix)
        Inverse_matrix
}
