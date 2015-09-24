## The makeCacheMatrix function will only make and store a list of 4 functions:
## set, get, setinvMatrix, getinvMatrix

makeCacheMatrix <- function(x = matrix()) {
        
        invMatrix <- NULL
        
        # set is a function that will change the value of m put in the main function
        # and then initialise invMatrix to NULL because the previous one should be erase
        # If we stored makeCaceMatrix in a variable named m, we will have to type
        # m$set(anothermatrix)
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        # get is only here to return the value of m in case it has been changed by the set function
        get <- function() {
                x
        }
        
        # The purpose of setinvMatrix is to inverse the m matrix and to store it into
        # the variable called invMatrix
        setinvMatrix <- function(invMatrix) {
                invMatrix <<- solve
        }
        
        # getinvMatrix is the same than get an return the inverse matrix of 'm'
        getinvMatrix <- function() {
                invMatrix
        }
        
        list(set = set, get = get,
             setinvMatrix = setinvMatrix,
             getinvMatrix = getinvMatrix)
        
        
}


## cacheSolve takes as input the same matrix m than before

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        
        ## The first thing is to take the value of the inverse matrix if it has 
        ## been stored by setinvMatrix
        invMatrix <- m$getinvMatrix()
        
        ## Then the function checks whether the inverse matrix is null, if not,
        ## the function returns the inverse matrix which has been stored
        ## If invMatrix is not null, the function ends avec the return
        if(!is.null(invMatrix)) {
                message("getting cached inverse matrix")
                return(invMatrix)
        }
        
        ## If the invMatrix was null, the function takes the value of the m matrix
        ## in input, and store it locally in the data variable
        data <- m$get()
        
        ## then calculate the inverse matrix of m
        invMatrix <- solve(data, ...)
        m$setinvMatrix(invMatrix)
        invMatrix
}
