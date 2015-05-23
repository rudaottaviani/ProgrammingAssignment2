## These functions have the purpose to provide a caching mechanism to avoid  
## calculating the inverse of a matrix more than once. 
## The inverse of a matrix is a time consuming task and a caching mechanism
## can drammatically reduce the resources that this task requires.
##
##
## Usage Sample:
##
## > k <- matrix(...         # define a matrix by using the "matrix" function 
##                           # or other mechanism
## > m <- makeCacheMatrix(k) # create a "caching" container for the "k" matrix
##                           # by using "makeCacheMatrix"
## > s <- cacheSolve(m)      # the inverse of k is calculated and the result is
##                           # cached. From that moment onwards, "cacheSolve" is invoked, the 
##                           # inverse calculation is skipped and the cached 
##                           # value is returned


## This function accepts a matrix as input parameter, and returns a list of 
## functions able to manipulate the matrix and its inversed in the cache. 
## Whenever the function "setMatrix" is invoked the cache is reset.

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


## this function is used to calculate the inverse of a matrix and cache the 
## result by using "makeCacheMatrix" . if the inverse is already in the 
## cache the function returns the cached value instead of calculating it.
## "..." args are forwarded to the "solve" function exactly as they were.

cacheSolve <- function(x, ...) {
        cachedInvertedMatrix <- x$getInvertedMatrix()
        
        # if the cached value exists, the value is returned
        if(!is.null(cachedInvertedMatrix)){
                message("Using Cache Data")
                return(cachedInvertedMatrix)
        }
        
        # the cached value does not exists. The inverse of the matrix is
        # calculated and assigned to a temp value
        invertedMatrix <- solve(x$getMatrix(), ...)
        
        # the cached value is inserted into the cache
        x$setInvertedMatrix(invertedMatrix)
        
        invertedMatrix
}
