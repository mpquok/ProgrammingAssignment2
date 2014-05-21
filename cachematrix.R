## Put comments here that give an overall description of what your
## functions do

## make cache matrix creates a list that contains functions to get and set the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #this function sets x and m to y and NULL respectively in a non-local environment (not the local
        #environment of the makeCacheMatrix function)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  #set the value of get to x
        setinverse <- function(inverse) m <<- inverse #store the cached inverse in m
        getinverse <- function() m #return m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## returns the inverse of a matrix defined by the makecachematrix
## checks to see if the inverse is cached; if so, returns cache value
## otherwise calculates the inverse and stores the answer in the cache

cacheSolve <- function(x, ...) {
        #set m locally using the getinverse function of the makecachematrix object
        #if m is not null returns the inverse matrix solution stored in m
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #set data to the matrix value stored in the makecachematrix object
        data <- x$get()
        #calculate the inverse of the matrix and store it locally as M
        m <- solve(data, ...)
        #store m in the makecachematrix object to cache inverse
        x$setinverse(m)
        #return the calculated inverse
        m
}
