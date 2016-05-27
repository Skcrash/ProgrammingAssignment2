## This pair of functions caches the inverse of a matrix allowing it to be computed once 
## rather than repeatedly.


## makeCacheMatrix makes a list to set the value of the matrix, get the value of the matrix, 
## sets the value of the inverse matrix, and gets the value of the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                y <<- x
                x <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv_mat) inv <<- inv_mat
        get_inverse <- function() inv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve first checks to see if the inverse has already been solved,
## if it has the previously calculated inverse is returned
## if not the inverse is calculated, and returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
