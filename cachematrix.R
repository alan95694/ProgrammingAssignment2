## Set of functions calculate and cache a matrix inversion 
## Coded 2/20/16 as part of Coursera R class, week 3.

# This function starts it all, user should give it a nice
#  square invertable matrix and save the output to be used
#  with cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    ## Return a matrix that is the inverse of 'x'
    mat <- NULL # ensure cached matrix is empty
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setInv <- function(inv) mat <<- inv
    getInv <- function() mat
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}

# Called by user after makeCacheMatrix with special vector 'x'
#  checks to see if mat-inv has already been calcuated, if so 
#  the last mat-inv is returned.  Else a new mat-inv is found 
#  and returned.
cacheSolve <- function(x, ...) {
    
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
