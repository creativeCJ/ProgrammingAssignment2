# Example
# Caching the Mean of a Vector (Given: Assignment Notes)

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
    setmean = setmean,
    getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


# Exercise

# makeCacheMatrix
# (Given: Assignment Notes) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {           # here x is set equal to an empty matrix
    Ix <- NULL        # here inverse is set equal to NULL
    set <- function(y){
        x <<- y        # here the set function assigns the argument to x
        Ix <<- NULL      # when set function is called, Inverse (Ix) is re-set to NULL
    }
    get <- function() x       # function as before and this function returns the matrix
    setInverse <- function(solve) Ix <<- solve        # setInverse overrides the previous value of Ix and assigns the argument to Inverse
    getInverse <- function() Ix        # getInverse returns the Inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  # creates a list of the functions
}

# cacheSolve
# (Given: Assignment Notes) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    Ix <- x$getInverse()
    if(!is.null(I)){
        message("getting cached data")
        return(Ix)           # If the value of Inverse is NOT null, cacheSolve returns that value
    }
    # If the value of Inverse is NULL, then you retrive matrix x and calculate the inverse with the solve() function
    message("newly calculating data")
    data <- x$get()
    Ix <- solve(data, ...)
    x$setInverse(Ix)           # Sets Inverse as the calculated value
    Ix           #Gives the Inverse value
}
