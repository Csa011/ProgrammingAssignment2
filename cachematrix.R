## A short scirpt to cache a matrix and its inverse in order to avoid computing it repeatedly

## The first function creates a list able to cache the value of the matrix and its inverse.
## Note that the inverse is not calculated, but can be added assigning the value to x$setinv

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL     
    
    set <- function(y) {     # Set the value of the matrix
    
      x <<- y                # If matrix is set through x$set(), then value of y is assigned to x in parental environment
      
      inv <<- NULL
    
    }
    
    get <- function() x      # Get the value of the matrix
    
    setinv <- function(inverse) inv <<- inverse   #Set the value of the inverse of the matrix
    
    getinv <- function() inv   #Get the value of the inverse. If not defined through x$setinv, then it will be NULL                  
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)   # List containing the functions
 
}


## Calculate the inverse of the matrix previosly created, but first check if the inverse is already
## in the cache, i.e. if x$getinv returns already a value

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()     #retrieves & assigns inverse cache value
        
        if(!is.null(inv)) {   #if the inverse cache value exists, prints a message and the value
        
              message("getting cached data")
        
              return(inv)
        }
  
        ## If the inverse cache value does not exist 
  
        data <- x$get()           # Assigns the cache matrix
  
        inv <- solve(data, ...)   # Calculates the inverse
  
        x$setinv(inv)             # Assign the inverse to first function x$setinv, so now is in cache
  
        return(inv)               # and returns calculated inverse 
  
}
