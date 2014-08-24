## This pair of functions can cache the inverse of a matrix.


## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function( m = matrix() ) {
        i <- NULL
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        # Returns the value of the original matrix.
        get <- function() {
                m
        }
        #called by cacheSolve stores matrix inverse by superassignment. 
        setInverse <- function(inverse) {
                i <<- inverse
        }
        # Method that returns cached value on subsequent accesses. 
        getInverse <- function() {
                # Returns the inverse property from superassignment
                i
        }
        # Functions must be listed to allow for external access. 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Solves for the inverse matrix and stores it 
## or returns the inverse matrix if already cached. 

cacheSolve <- function(x, ...) {
        #Accessess x (the matrix) and gets the inverse
        m <- x$getInverse()
        #If the inverse was not null (already cahched) retrun inverse. 
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        #if not we get the original matrix
        data <- x$get()
        # Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        #We then set the inverse of the matrix.
        x$setInverse(m)
        #Then we return the matrix
        m
}