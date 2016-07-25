

#makeCacheMatrix
#This first part inverts a matrix
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#  solve  if X is a square invertible matrix, then solve(X) returns its 
#  inverse.

#This part checks to see if the inverse has already been taken and if it has # it uses it
#cacheSolve
cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

zerotenmatrix <- diag(10,3)
 cachematrix <- makeCacheMatrix(zerotenmatrix)
cachesolve(cachematrix)

#after running the above code - to check if it is cached run this cmd again
# cachesolve(cachematrix)
