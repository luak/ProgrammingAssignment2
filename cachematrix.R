# define simple matrix
X = matrix(c(1,2,3,4), nrow = 2, ncol = 2)

makeCacheMatrix <- function(x = matrix()) {
    # this function creates complex object consits of several methods and 'variables'
    # (matrices), it is something like class in OOP
    
    m <- NULL
    # setting variables
    set <- function(y) {
        # for original matrix
        x <<- y
        # for inverse matrix
        m <<- NULL
    }
    
    # this function returns original matrix
    get <- function() x
    
    # put inverse matrix into the instance of the object
    setmat <- function(solve) m <<- solve
    
    # returns inverse matrix 
    getmat <- function() m
    
    # complex output from makeCacheMatrix
    list(set = set, get = get,
         setmat = setmat,
         getmat = getmat)
}

# Create 'instance' of the 'class' makeCacheMatrix 
x = makeCacheMatrix(X)

# Call the method: return original matrix X
x$get()

cacheSolve <- function(x, ...) {
    # practical working with the istance of the makeCacheMatrix
    # try to get (from instance x) inverse matrix if it exists, otherwise return NULL
    m <- x$getmat()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if inverse matrix doesn't exist, solve one
    data <- x$get()
    m <- solve(data)
    # put the inverse matrix into the object
    x$setmat(m)
    # return the matrix that is inverse of 'X'
    m
}

# solve inverse matrix
cacheSolve(x)
# get cashed inverse matrix
cacheSolve(x)