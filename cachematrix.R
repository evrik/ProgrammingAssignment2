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
#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then the
#`cachesolve` should retrieve the inverse from the cache.
cachesolve <- function(x, ...) { 
#loading data stored in "getsolve" from previous function
        m <- x$getsolve() 
#if there are data in m that function print the message and data from variable m
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
#otherwise matrix is loaded from "get" subfunction to data
        data <- x$get()
        m <- solve(data, ...) #calculate inverse matrix and store result in m
        x$setsolve(m)  # write result from m into the makeCacheMatrix function
        m #returns result
}
