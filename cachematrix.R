## in order to cache the teh functions and results for calculating 
## invers of a matrix

## makeCachematrix  prepares a matrix to cache its results 

makeCacheMatrix <- function(x = matrix()) {
    ###build the function list for the matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) m <<- solve  #<<-, they force us to use m from the env
    
    getsolve <- function() m #looks for the results in the cache
    
    list(set = set, #create the list x, with the functions
         get = get, 
         setsolve = setsolve,  #setsolve find the inv of matrix,
         getsolve = getsolve)  #looks for the results in the cache
}




## chache Solve check the chache for results and if not present ##calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'###if solution found in cache, use, esle calculate another
    m <- x$getsolve()
    if(!is.null(m)) {
        message("Getting MY Cached Matrix")
        return(m) #return results if found
    }
    data <- x$get()
    m <- solve(data, ...) #solve the matrix
    x$setsolve(m) #using the setsolve funtion, member of the list x
    print(m)
}

p <- matrix (1:4, 2, 2)



q <- makeCacheMatrix(p)
cacheSolve(q)
cacheSolve(q)
cacheSolve(q)