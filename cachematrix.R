## These functions deal with a special type of matrix objects that can
## cache their inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special matrix object that can also cache its inverse.
    ## The object is a list containing four function:
    ##1. set the value of the matrix
    ##2. get the value of the matrix
    ##3. set the value of the inverse
    ##4. get the value of the inverse
    ## 
    ## The input must be a matrix (in the sense of is.matrix() ).
    
    inv <- NULL  #Initialse inverse to NULL until it's calculated.
    
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(input.inv){inv <<- input.inv}
    getinv <- function(){inv}
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}




cacheSolve <- function(x, ...) {
    ## This function takes a special CacheMatrix object as above and returns a 
    ## CacheMatrix object with the inverse matrix. If the inverse has already
    ## been calculated and cached, the inverse is retrieved from the cache.
    ## If not, the inverse is calculated and then cached in the object.
    
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data")
    }else{
        message("Calculating inverse:")
        inv <- solve(x$get(),...)
        x$setinv(inv)
    }
    
    inv #Return inverse        
}