## A pair of functions that cache the inverse of a matrix
##Assumption: The matrix supplied is a square invertible matrix


## makeCacheMatrix function creates a special matrix object that can cache its inverse.
##It returns a list containing four functions - set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ##this variable stores the inverse of the matrix
        
        ##set function sets the matrix to what is passed to it and sets the inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get function returns the matrix
        get <- function() x
        
        ## setinverse function sets the value of the matrix inverse i.e. inv variable
        setinverse <- function(solve) inv <<- solve
        
        ## getinverse returns the value of the matrix inverse i.e. inv variable
        getinverse <- function() inv
        
        ## returning the list containing the four functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve function computes the inverse of the special matrix created by makeCacheMatrix function.
##If inverse is already calculated and matrix is not changed, then it retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrieve inverse value from cache if it is already calculated
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting matrix inverse from cache")
                return(inv)
        }
        
        ## calculate inverse if it is not already caculated
        data <- x$get()
        inv <- solve(data, ...)
        
        ## set the value of the inverse i.e. inv variable
        x$setinverse(inv)
        inv
        
}

