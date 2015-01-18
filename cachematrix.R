## makeCacheMatrix() and cacheSolve() provide the ability to create an
## object that stores a matrix and its inverse, which eliminates the
## need to repeatedly compute a matrix inverse
## To use, first call makeCacheMatrix with an invertible matrix as its argument
## Example: mObj <- makeCacheMatrix(matrix(c(1,2,3,4),ncol=2,nrow=2))
## then call cacheSolve with the newly created object: mInv <- cacheSolve(mObj)


##  Function: makeCacheMatrix
##   Purpose: initializes an object that caches a matrix and its inverse
## Arguments: an invertable matrix -- default creates an empty matrix
##     Usage: mObj <- makeCacheMatrix(m)
##     Notes: pass an invertible matrix, or an empty matrix will be created by
##            default -- it is possible to use the "set" function to change the
##            matrix after creating the initial object, e.g., mObj$set(m)

makeCacheMatrix <- function(x = matrix()) {
    m   <-  NULL
    set <-  function(y) {
        x  <<- y
        m  <<- NULL
    }
    get    <-  function() x
    setinv <-  function(matinv) m <<- matinv
    getinv <-  function() m
    list(set    = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##  Function: cacheSolve
##   Purpose: returns the inverse of a cached matrix object
## Arguments: an object created by the makeCacheMatrix function
##     Usage: mInv <- cacheSolve(mObj)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m    <- solve(data, ...)
    x$setinv(m)
    m
}
