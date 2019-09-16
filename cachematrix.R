## The functions provided here provide a means of computing the inverse value
## of a matrix, storing the value in a cache, and returning the cached value
## if the inverse is requested again.


## makeCacheMatrix: The function provides a list of functions, to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the matrix inverse
## - get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                          # variable to hold matrix inverse
        set <- function(y) {               # Given matrix y,
                x <<- y                    # save the matrix to x
                m <<- NULL                 # set the inverse variable to NULL
        }
        get <- function() x                # Function to get the matrix value
        set_inv <- function(inv) m <<- inv # Function to set the inverse value
        get_inv <- function() m            # function to get the inverse value
        list(set = set, get = get,         # list returned with functions  
             set_inv = set_inv,
             get_inv = get_inv)
        
}


## cacheSolve: The function calculates the inverse of a square matrix
## If the inverse has already been calculated, the function provides the 
## inverse value

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()                     # get the cached inverse value
        if(!is.null(inv)) {                      # if not NULL
                message("getting cached data") # Say so
                return(inv)                      # return the cached value
        }
        
        # If we get here, the inverse value has not been set
        mat <- x$get()                         # Get the matrix
        inv <- solve(mat)                      # solve for the inverse
        x$set_inv(inv)                           # cache the inverse value
        inv                 ## Return a matrix that is the inverse of 'x'
        
        
}
