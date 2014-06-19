## Put comments here that give an overall description of what your
## functions do

## This function stores four functions that works on the matrix x
##in a list that is returned when the function is called to create
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() x
    
    set <- function(m){
        x <<- m
    }
    
    getInv <- function() inv
    
    setInv <- function(m) inv <<- m
    
    list(set=set, get=get, getInverse=getInv, setInverse=setInv)

}


## This function first checks if the inverse has been calculated if so it returns
## the cached value else it compute the value cache it and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solv <- x$getInverse()
        
    if(is.null(solv)){
        mat <- x$get()
        solv <- solve(mat) ## Here is where it calculates the Inverse
        x$setInverse(solv) ## Here it stores it
    }
    
    print(solv)    ## One return for both cases
}
