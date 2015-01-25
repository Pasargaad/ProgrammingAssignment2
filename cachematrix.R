## function 'makeCacheMatrix' creates a special matrix 'x', which is a list containing a function to 
## 1. set the value of the matrix, 
## 2. get the value of the matrix,
## 3. set the value of the matrix inverse, and
## 4. get the value of the matrix inverse.
## 'makeCacheMatrix' function can cache its inverse in co-operation with the function 'cacheSolve' defined later.  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## the following local function sets the value of the matrix.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## the following local function is defined to get the value of the matrix.
        get <- function() x
        ## the following local function caches the value of the 
        ## matrix inverse (that is calculated by fucntion 'cacheSolve').
        setinverse <- function(inverse) inv <<- inverse 
        ## the following local function is defined to get the value of the matrix inverse from the cache.
        getinverse <- function() inv
        ## returns a list containiing the four local functions defined above.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## function 'cacheSolve' calculates and returns the inverse of the special matrix created by 'makeCacheMatrix' function above.
## It first checks if the matrix inverse has already been calculated (and the matrix has not changed). 
## If so, it fetches the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix, 
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## calls the local function 'getinverse' defined in 'makeCacheMatrix' and writes the returned result to 'inv'
        inv <- x$getinverse()
        ## if matrix inverse is already in the cache ('inv' is not NULL), 
        ## then its value is fetched from the cache without re-computation.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if the matrix inverse is not in the cache ('inv' is NULL), 
        ## the matrix is written to 'data' by calling the local function 'get' 
        data <- x$get()
        ## the matrix inverse is calculated using R function 'solve' and is written to 'inv' 
        inv <- solve(data, ...)
        ## the local function 'setinverse' that is defined in 'makeCacheMatrix' is called 
        ## to copy the calculated value of the inverse matrix into the cache   
        x$setinverse(inv)
        inv
}
