## Put comments here that give an overall description of what your
## functions do

## The two functions makeCacheMatrix and cacheSolve allow you
## to cache the inverse of a matrix, avoiding repeated
## recalculation whenever the inverse is needed.



## makeCacheMatrix: creates a “container” for a matrix and its inverse,
## providing four interface functions:
##  - set(x): sets a new matrix and resets the cached inverse
##  - get(): returns the current matrix
##  - setInv(i): caches the computed inverse i
##  - getInv(): returns the cached inverse (or NULL if not yet computed)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                     
    set <- function(y) {
        x <<- y                     
        inv <<- NULL                
    }
    get <- function() x             
    setInv <- function(inverse) inv <<- inverse  
    getInv <- function() inv  
    list(
        set = set,
        get = get,
        setInv = setInv,
        getInv = getInv
    )
}



## cacheSolve: computes (or retrieves from cache) the inverse of
## the special “matrix” created by makeCacheMatrix. If the inverse
## has already been calculated, it returns it immediately; otherwise
## it computes it via solve(), caches it, and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()              
    if (!is.null(inv)) {
        message("getting the data already in the cache")
        return(inv)               
    }          
    inv <- solve(x$get(), ...) #this function automatically calculates the inverse
    x$setInv(inv)          
    inv            
}
