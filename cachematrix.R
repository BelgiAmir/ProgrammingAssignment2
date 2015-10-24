
## This makeCacheMatrix function gets as parameter a matrix x
## and returns a list of 4 elements, each element is a function where
# Function #1 - sets the value of x 
# Function #2 - gets the value of x
# Function #3 - sets the value of the inverse of x
# Function #4 - gets the value of the inverse of x

							##creating a matrix 
makeCacheMatrix <- function(x = matrix()) 
{
		##The variable that holds the inverse of x
        m_inv <- NULL
		
		##The Set function 
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
		
		##The get function - returns x
        get <- function() x
		
		##The set inverse function  
        setInverse <- function(inverse) m_inv <<- inverse
		
		##The get inverse function  
        getInverse <- function() m_inv
		
		##The LHS is the name of the parameter
		##The RHS is the value - in each line it is a function that we defined previously  
        list(set = set,
			 get = get,
             setInverse = setInverse,
			 getInverse = getInverse)

}


## The cacheSolve function receives an object x that was returned from the makeCacheMatrix function 
## If the above parameter holds the inverse of the matrix inside - we return it, otherwise we calculate it and set it in x
## So it can be retrieved by a later call

cacheSolve <- function(x, ...) {
		
		##Trying to get the inverse 
		m_inv <- x$getInverse()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
		
		##Inverse was not found (e.g never calculated)
        data <- x$get() ##Getting the matrix inside of x
        m_inv <- solve(data, ...) ##Calculating its inverse
        x$setInverse(m_inv) ##Setting the value of the inverse in x so we can access it in later call
        m_inv ##returning the inverse 
}
