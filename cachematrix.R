## The following functions will create a special list which stores functions
## related to a cached matrix and then compute its inverse.  

## This very first function will create the list containing the functions to
## set the value of the matrix, get the value of the matrix, set the value of 
## the inversed matrix and get the value of the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initially setting the matrix to NULL
        m <- NULL
        
        ## Creating the functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(solve) {
                m <<- solve
        }
        getinverse <- function() {
                m
        }
        
        ## Setting the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Now, this function will calculate the inverse of the matrix. If it has 
## already been calculated, it will return its value. If not, then it will 
## calculate and cache the inversed matrix.

cacheSolve <- function(x, ...) {
        
        ## Getting the list
        m <- x$getinverse()
        
        ## Checking if the inverse has already been calculated
        if(!is.null(m)) {
                
                ## Returning a message followed by the inversed matrix
                message("Getting the cached data...")
                return(m)
        }
        
        ## If the above is not true, calculating the inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
