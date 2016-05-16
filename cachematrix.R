## Function to support Inversion of Matrix and cache the result 
##(Inverted Matrix) to be used later. 

## This function creates a special Matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    set <- function(y){
        x<<-y
        mInverse<<-NULL
    }
    get<-function() x
    setInverse<-function(iv) mInverse <<- iv
    getInverse<-function() mInverse
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
}


## This function returns Inverse of a square Matrix. If the Inversion of Matrix 
##is already available in cache returns the same value. If not calulates the 
## Inversion of the Matrix,saves it in cache and returns it.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iv <- x$getInverse()
    if(!is.null(iv)){
        message("getting cached data")
        return(iv)
    }
    iv <- solve(x$get())
    x$setInverse(iv)
    iv
}
