## This function creates a special "matrix" object that can cache its inversehead
makeCacheMatrix <- function(x = matrix()) {
                 m <- NULL # add internal variable
        set <- function(y) { 
#subfunction takes the matrix and assigns it to a variable x 
# then the subfuction clears the cache (m)
# we use <<- operator to clean x and m variables in parental enviroment as well
                x <<- y
                m <<- NULL
        }
        get <- function() x # "get" subfunction takes matrix, that was stored in x and returns it.
        setsolve <- function(solve) m <<- solve #subfuction calculates inverse matrix and stores it in setsolve 
        getsolve <- function() m #subfunction just return cached(stored) inverse matrix
# creating a list of the 4 subfunctions defined before to use them later from cachesolve function        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
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
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
