## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates the CacheMatrix object using as input a conventional matrix

makeCacheMatrix <-function(x){
       i<-NULL
       set <- function(y) {
                x <<- y
                m <<- NULL
       }
       get <- function() x
       setinv <- function(solve) i <<- solve
       getinv <- function() i
       list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of a CacheMatrix object. If the inverse is not previously cached, the function calls solve to calcuate the inverse that is then cached. If the inverse matrix is already cached, the function simply returns it.

cacheSolve <- function(x) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

