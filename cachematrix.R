## This functions has the purpose to provide a caching mechanism to avoid  
## calculating the inverse of a matrix 2 or more time. 
## The inverse of a matrix ia a time consuming task and a caching mechanism
## can drammatically reduce the resources used by this task.
##
##
## Usage Sample:
##
## > k <- matrix()           #define a matrix using the matrix function or other 
                             #mechanism
## > m <- makeCacheMatrix(k) #using makeCacheMatrix create a "caching" container
##                           #for the k matrix
## > s <- cacheSolve(m)      #the inverse of k is calculated and the result is
##                           #cached. The next time cacheSolve is invoked the 
##                           #inverse calculation is skipped and the cached value
##                           #is returned

## This function accept as input paramtere a matrix and return a list of 
## functions used for manipulate the matrix and the inverse cache. every
## time the function "setMatrix" is invoked the cache is reset.

makeCacheMatrix <- function(m = matrix()){
        invertedMatrix <- NULL            
        
        # Set a new matrix and "clean" the cache
        setMatrix <- function(newMat){
                m <<- newMat
                invertedMatrix <<- NULL
        }
        
        # Return the original matrix
        getMatrix <- function(){
                m
        }
        
        # Set the inverted matrix into the cache
        setInvertedMatrix <- function(solve){ 
                invertedMatrix <<- solve
        }
        
        # Get the inverted matrix from the cache
        getInvertedMatrix <- function(){
                invertedMatrix  
        }
        
        list(
                setMatrix=setMatrix, 
                getMatrix=getMatrix,
                setInvertedMatrix=setInvertedMatrix,
                getInvertedMatrix=getInvertedMatrix
        )
}


## this function is used to calculate the inverse of a matrix and using 
## makeCacheMatrix the result is cached. if the inverse is already in the 
## cache the function return the cached value insted calculate the inverse.
## ... arguments are passed to the solve function.

cacheSolve <- function(x, ...) {
        cachedInvertedMatrix <- x$getInvertedMatrix()
        
        # if exists return the cached inverted matrix
        if(!is.null(cachedInvertedMatrix)){
                #message("Using Cache Data")
                return(cachedInvertedMatrix)
        }
        
        # the cached value does not exists. The inverted matrix has been
        # calculated and assigned to a temp value
        invertedMatrix <- solve(x$getMatrix(), ...)
        
        # the cached value is inserted into the cache
        x$setInvertedMatrix(invertedMatrix)
        
        invertedMatrix
}
