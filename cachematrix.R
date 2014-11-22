## makeCacheMatrix creates a list object containing (x,m) this object will be used to store the 
## reult calculated by cacheSolve. This function also holds the get and set functions for the list objet.
## cacheSolve checks the matrix object passed as an argument and if makeCacheMatrix has created a list object for the argument
## calls the getInverse function from makeCacheMatrix, retrevies the cahed object and displays it, if not calls the setInverse function from makeCacheMatrix.


## Creates and object of type list, with a matrix passed as the function argument x 
 ## and another object m, which is used to hold the inverse value to be calculated 
 ##by the cacheSolve function, m is initialised to NULL.

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL 
    set <- function(y) {  ## set default values for special object list(x,m)
        x <<- y 
        m <<- NULL 
    } 
    get <- function() x 
    setInverse <- function(solve)  m <<- solve ##assigns values to list
    getInverse <- function() m  ##returns list
    list(    set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse) 
}


## calls the get inverce function from makeCacheMatrix,if m is not null get the cached object and return the cahed value. 
## If m is null create a new list object with the argument passed to the function 
## and inverse value returned by the solve function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() 
        if(!is.null(m))  
            {  
            message("getting cached data") 
            return(m) 
        }
    data <- x$get() 
    m <- solve(data, ...) 
    x$setInverse (m) 
    
    m  
}