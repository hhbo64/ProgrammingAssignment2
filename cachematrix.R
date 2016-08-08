## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setcachInv <- function(inInv) inv <<- inInv
        
        getcachInv <- function() inv
        
        list(set=set,get=get,setcachInv = setcachInv,getcachInv=getcachInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getcachInv()
        if (!is.null(inv)){
                message("Get cached inverse")
                return (inv)
        }
        else{
        ## Return a matrix that is the inverse of 'x'
        inv <- solve(x$get())
        x$setcachInv(inv)
        return(inv)
        }
}
