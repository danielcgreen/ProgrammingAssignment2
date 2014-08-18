## Finding the inverse of a matrix can be an expensive operation, especially when repeated.
## The makeCacheMatrix and cacheSolve functions provide a way to cache a matrix's inverse, thereby
## limiting the number of times the inverse must be calculated.


## The makeCacheMatrix function returns a list of functions that facilitate the caching of
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Intialize the solved value to NULL
        solved <- NULL 
        
        # Return the matrix supplied
        get <- function() x
        
        # Save the supplied matrix
        set <- function(y){
                x <<- y
                
                # This function assumes that a different matrix has been stored and thus
                # the original cached inverse is incorrect. Overwrite any stored values with NULL.
                solved <<- NULL
        }
        
        # Store the supplied matrix inverse in the solved variable
        setsolved <- function(x){
                solved <<- x
        }
        
        # Retrieve the matrix inverse from the solved variable
        getsolved <- function() solved
        
        # Return a list of functions
        list(get=get, set=set, setsolved=setsolved, getsolved=getsolved)
}


## The cacheSolve function returns the inverse of the matrix supplied by either retrieving
## the inverse from the cache or calculating and storing the inverse in the cache.  This
## function expects a list in the form of the output of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        # Attempt to retrieve the solved matrix from the cache
        solved <- x$getsolved()
        
        # Test to see if the matrix has been solved.
        if(!is.null(solved)){
                # The matrix has been solved.  Retrieve it from cache and return.
                message("getting cached data")
                return(solved)
        }
        
        # The matrix has not been solved.  Retrieve the data, solve the matrix
        # and store it in the cache.
        data <- x$get() # Retrieve data
        solved <- solve(data) # Solve the matrix
        x$setsolved(solved) # Store the solved matrix in the cache for future use
        solved # Return the solved matrix
}
