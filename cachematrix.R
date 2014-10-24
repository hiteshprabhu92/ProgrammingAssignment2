## This R program is designed to calculate the inverse of a matrix and cache
## it if necessary. And if the same matrix is passed twice, the function  
## caches the inverse of the matrix to reduce redundancy in calculations.

## *********************************************************************

## makeCacheMatrix creates an object which is essentially a list that 
## contains functions to set and get the value of a matrix, and also
## to set and get value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## intializing the variable which holds inverse to null
    
    set <- function(y) {
        x <<- as.matrix(y)
        i <<- NULL
    } ## set assigns the value of the new matrix to x
    
    get <- function() x ## Is used to read the present matrix
    
    setinverse <- function(inverse) i <<- inverse ## Assigns the result
    ## of cacheSolve to i. <= In essense, caching.
    
    getinverse <- function() i ## Returns the present cache value of i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) ## This is the creation of the object
    ## that contains the list of values described above.
}


## cacheSolve is the function that calculates the inverse of the matrix
## unless the inverse for the present matrix already exists in the cache
## (i.e., due to a previous calculation)

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse() ## Reading present value of inverse matrix's cache
    
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    } ## Checking if the inverse for the matrix is already cached, and 
    ## returing cached inverse of 'x' if it is.
    
    data <- x$get() ## To read the matrix assigned  
    
    i <- solve(t(data)) ## Calculates inverse of the matrix
    
    x$setinverse(i) ## To assign calcualted value of inverse to cache
    
    return(i) ## Return a matrix that is the inverse of 'x'
}
