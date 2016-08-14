## The following two functions enables us to
## cache the inverse of a matrix

## The function creates a special matrix object that can
## cache its inverse

##This function particularly does the following
##1)set the value of the matrix
##2)get the value of the matrix
##3)set the value of the matrix inverse
##4)get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y){
        # <<- operator assigns a value to an object in an environment that
        ## is different than the current environment
        x <<- y 
        inv<<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) inv<<- inverse
    getInverse <- function() inv
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,       
         setInverse = setInverse,   
         getInverse = getInverse)        
    
}


## The function computes the inverse of matrix x returned by the  
## function defined above (makecacheMatrix)
## If the inverse has already been computed, then the cachesolve function
## retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
        ## The function returns a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    ##if the inverse has already been computed, we retrieve the inverse
    ## from the cache through the following code
    if(!is.null(inv)){                  
        message("getting cached data")  
        return(inv)                       
    }
    #otherwise compute the inverse
    mat <- x$getMatrix()
    inv <- solve(mat)
    #sets the value of inverse in teh cache
    x$setInverse(inv)
    ##The following code returns the inverse of the matrix
    inv                 
}
