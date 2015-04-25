# Assignment wk2
# y<-c(7,1,3,4)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# a<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
# 
# cacheSolve(a)
# a$set(matrix(5:8,nrow=2,ncol=2))


# a$getinverse()
# a$setinverse()
# a$set(c(7,1,3,4))

