# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# --------------------------------
# Sample
# --------------------------------
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create special matrix
# > cx$get()                                  // Return matrix
# > cacheSolve(cx)                            // Return inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return cached inverse

makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        # Return the matrix with our newly defined functions
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
#
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # If the inverse is already calculated, return it
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        # If the inverse is not calculated, Calculate it
        data <- x$get()
        inv <- solve(data)
        # Cache inverse.
        x$setinverse(inv)
        # Return 
        inv
}