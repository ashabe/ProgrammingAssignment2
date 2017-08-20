## we want to return: a list containing functions that will set and get the matrix,set and get th inverse
## z is a square invertible matrix    


makeCacheMatrix <- function(z = matrix()) {
        
        
        
        invert = NULL
        set = function(y) {
                # we use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                z <<- y
                invert <<- NULL
        }
        get = function() z
        setinv = function(inverse) invert <<- inverse 
        getinv = function() invert
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## z is the output of makeCacheMatrix()
## we want to return the inverse of the original matrix input to makeCacheMatrix()


cacheSolve <- function(z, ...) {
        
        
        invert = z$getinv()
        
        
        if (!is.null(invert)){
                 
                message("getting cached data")
                return(invert)
        }
        
        # calculate the inverse 
        mat.data = z$get()
        invert = solve(mat.data, ...)
        
        # setting the value of the inverse in the cache
        z$setinv(invert)
        
        return(invert)
}
