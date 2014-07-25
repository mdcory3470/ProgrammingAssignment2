# Two functions that cache an inverse of a matrix

# Creates a inverse matrix object
makeCacheMatrix <- function(x = matrix()) {

		# Initialize the inverse
        IM <- NULL
		
		# Set matrix
        set <- function(y) {
                x <<- y
                IM <<- NULL
        }
		
		# Get matrix
        get <- function() x
		
		# Set the inverse 
        setIM <- function(I_mat) IM <<- I_mat
		
		# Get the inverse
        getIM <- function() IM
		
		# Create method list
        list(set = set, get = get,
             setIM = setIM,
             getIM = getIM)
}

# Calculates the inverse of the matrix in makeCacheMatrix and retrieves 
# the inverse from cache if it has already been calculated.
cacheSolve <- function(x, ...) {

		# Get the inverse from x
        IM <- x$getIM()
		
		# Return the inverse from cache if already computed
        if(!is.null(IM)) {
                message("getting cached data")
                return(IM)
        }
		
		# Get the matrix from x
        data <- x$get()
		
		# Make the inverse computation
        IM <- solve(data)
		
		# Set the inverse in x
        x$setIM(IM)
		
		# Return the inverse matrix
        IM
}