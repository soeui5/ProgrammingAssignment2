## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ##set matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
                }
        ##get matrix
        get <- function() x
        ##set inverse matrix
        setInverse <- function(solve) i <<- solve
        ##get inverse matrix
        getInverse <- function() i
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        }

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        ## if statement for getting data
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
        ##get data
        data <- x$get()
        i <- solve(data) %*% data
        x$setInverse(i)
        }
