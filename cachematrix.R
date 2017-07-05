## makeCacheMatrix and cacheSolve are a pair of fuctions that 
## cache the inverse of a matrix

## makeCacheMatrix is a function that creates matrix "x" within function y and sets the inverse to NULL.  
## Additionally, this function defines the get, setinverse, and getinverse functions which call the matrix and inverse. 
## Lastly, these functions are defined by names and can be accesed within the environment of the makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set <- function(y){
              x<<-y
              i<<-NULL
    }
    get <-function() x
    setinverse <-function(inverse) i <<-inverse   
    getinverse <-function()i
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates and stores the inverse of matrix x if it has not already been defined and stored
## within the objects of makeCacheMatrix
cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)) {
              message("getting cached data")
              return (i)
        }
        data<- x$get()
        i<-solve(data)
        x$setinverse(i)
        i## Return a matrix that is the inverse of 'x'
}
