## This function utilizes the lexical scoping concept in R programming, and uses the caching ability to speed up the results while involving matrix calculations (in this case Matrix inverse)
## 

## The function makeCacheMatrix is used to get and set tmatrix, and functions getinverse and setinverse is used to get and set results of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
           inverse <- NULL 
           set <- function(y) {
                   x <<- y
                   inverse <<- NULL
           }
           get <- function() x
           setinverse <- function(invresult) inverse <<- invresult
           getinverse <- function() inverse
           list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This functions will return the inverse of the matrix if present in cache, Else, it will calculate the inverse using SOLVE function, and then return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invresult<- x$getinverse()
        if(!is.null(invresult)) {
                message("getting cached data")
                return(invresult)
        }
        input <- x$get()
        invresult <- solve(input, ...)
        x$setinverse(invresult)        	
        invresult
}
