## Write a pair of functions 'makeCacheMatrix' and 'cacheSolve'
## that cache the inverse of a matrix

## makeCacheMatrix is a function that creates a special matrix object that can
## cache its invers for the input which is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
        x<<-y
        inv<<-NULL
}
get<-function()x
setInverse<-function(inverse)inv<<-inverse
getInverse<-function()inv
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## cacheSolve fucntion computes the inverese of the special matrix
## returned by the makeCacheMatrix. The cacheSolve retrives the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}