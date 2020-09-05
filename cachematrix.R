## Put comments here that give an overall description of what your
## functions do

# The purpose of these functions are to explore fastest ways to do simple operation like 
# the inverse of a matrix. For very long vector it may take too long to compute the inverse
# of a matrix. Specially if we have to do this operation over and over again in a loop algorithm
# If the contents of the matrix are not changing, it result easiest and faster to cache 
# and display the inverse rather than recompute the calculation. 

## Write a short comment describing this function

# The objective of this function is to create a special "matrix" object 
# that can store her inverse. The function is composed of 2 parts: 
# the first one represent changes in the input data (set()) and how to deliver this data 
# to others environments (get()).
# The set function serves to eventually edit the data matrix and the get 
# function is for obtaining that matrix in the environment where it is called. 
# The second part, cache the inverse of the matrix (setinv()) and deliver 
# that cache whatever the function it's called (getinv())

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse){
                inv <<- inverse 
# If we don't use the double assignment operator, the inverse value returns to NULL (inv <- NULL),so there is no cache inverse in that case.                
# The double assignement safe the value in a upper environment, in this case "inv <- NULL" updates.                
        }
        getinv <- function() inv 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function

# The objective of this function is to calculate and deliver the inverse of the 
# matrix, set as input on makeCacheMatrix().
# The "delivery" that make this function can take 2 forms: 
# First, it calculates the inverse and returns the result only if this calculation
# wasn't made before. In other cases, just return the inverse of the matrix
# stored as a vector matrix cache created before (makeCacheMatrix())


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinv(inverse)
        inverse
}
