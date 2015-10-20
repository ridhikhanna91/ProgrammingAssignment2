# Programming Assignment 2-- 
#---------------------------------------------------------------------------------
# The two functions help in creating a matrix(assumed inverible)
# and solving for its inverse, if not available.
# If the inverse is already calculated, it saves the repeated computation and 
# directly caches it.
#---------------------------------------------------------------------------------

# makeCacheMatrix function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y) {
        x <<- y
        INV <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) INV <<- inverse
    getinverse <- function() INV
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
# If the inverse has already been calculated (with matrix unchanged), 
# it saves the additional compuation and directly retrieve the inverse
# from the cache.

cacheSolve <- function(x, ...) {
    INV <- x$getinverse()
    if(!is.null(INV)) {
        message("Getting CACHED data.")
        return(INV)
    }
    data <- x$get()
    INV <- solve(data)
    x$setinverse(INV)
    INV
}
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#    Test Output

# > sample<-rbind(c(1,2), c(2,1))
# > m<-makeCacheMatrix(sample)
# > m$get()
#    [,1] [,2]
#[1,]    1    2
#[2,]    2    1
#
# > cacheSolve(m)
#           [,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#
#
# > cacheSolve(m)
# Getting CACHED data.
#           [,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
 
