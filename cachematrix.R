## These functions are able to calculate and cache the inverse of a regular
## matrix. If the inverse has been calculated previously it has to be cached
## into an object of 'makeCacheMatrix' type.
## makeCacheMatrix creates a list type object where a regular matrix and its
## inverse calculated previously are cached in order to not to reclaculate
## it if is required. If the inverse has not been calculated, the
## corresponding espace is as NULL
makeCacheMatrix <- function(x = matrix(){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
 ## cacheSolve function is able to identify if into a 'makeCacheMatrix' object
 ## there is the inverse of a specific matrix cached and returned it as cached 
 ## data with no calculation necesity. If the inverse has not been calculated
 ## previously, it calculates the inverse and cached into the corresponding
 ## 'makeCacheMatrix' object to use it later, as need it.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat < x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}   
