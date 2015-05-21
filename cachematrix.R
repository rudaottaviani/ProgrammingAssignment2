## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()){
                invertedMatrix <- NULL            
        
        setMatrix <- function(newMat){
                        m <<- newMat
                        invertedMatrix <<- NULL
        }
        
        getMatrix <- function(){
                        m
        }
        
        setInvertedMatrix <- function(solve){ 
                        invertedMatrix <<- solve
        }
        
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                cachedInvertedMatrix <- x$getInvertedMatrix()
                
                if(!is.null(cachedInvertedMatrix)){
                        message("Using Cache Data")
                        return(cachedInvertedMatrix)
                }
                
                invertedMatrix <- solve(x$getMatrix(), ...)
                
                x$setInvertedMatrix(invertedMatrix)
                
                invertedMatrix
}
