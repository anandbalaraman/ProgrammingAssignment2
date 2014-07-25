## Below functions are used to compute and cache a matrix and its inverse

# Can be tested as:
# > mat<-makeCacheMatrix(matrix(1:4,2,2))       # Create matrix and cache using makeCacheMatrix()
# > mat$getMatrix()                             # Get matrix using getMatrix()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mat$getMatrixInv()                          # Try get matrix inverse using getMatrixInv()
# NULL                                          # Matrix inverse not cached
# > cacheSolve(mat)                             # Compute and cache matrix inverse using cacheSolve()
# Solving matrix inverse and caching it using makeCacheMatrix()
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mat$getMatrixInv()                          # Get matrix inverse using getMatrixInv()
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## Utility function for returning the following list of functions:
##      Getter and Setter functions of matrix
##      Getter and Setter functions of matrix inverse (does not solve matrix)
##
##      arguments   :   matrix()
##      returns     :   list of functions: getMatrix,setMatrix,getMatrixInv,setMatrixInv

makeCacheMatrix <- function(x = matrix()) {{
    
    # initalize inverse of matrix as NULL first
    matInv <- NULL
    
    # getter and setter functions for matrix
    getMatrix <- function() {
        x
    }
    setMatrix <- function(newMat) {
        x <<- newMat
        # when matrix set, its inverse must be reset
        matInv <<- NULL
    }
    
    # getter and setter functions for matrix inverse
    getMatrixInv <- function() {
        matInv
    } 
    setMatrixInv <- function(newmatInv) {
        matInv <<- newmatInv
    }
    
    # return list of getters and setters for matrix and its inverse
    return(list(getMatrix=getMatrix, setMatrix=setMatrix, 
                getMatrixInv=getMatrixInv, setMatrixInv=setMatrixInv))
}}

## Computes and returns inverse of a matrix
## and caches the result using the makeCacheMatrix() utility function
##      arguments   :   x, ...      (here we expect x to be a matrix)
##      returns     :   matrixInverse

cacheSolve <- function(x, ...) {
    
    matrixInverse <- x$getMatrixInv()
    
    if(!is.null(matrixInverse)) {
        message("Fetching inverted matrix data from makeCacheMatrix()")
        return(matrixInverse)
    }
    else {
        message("Solving matrix inverse and caching it using makeCacheMatrix()")
        tempMat <- x$getMatrix()
        matrixInverse <- solve(tempMat)
        x$setMatrixInv(matrixInverse)
        return(matrixInverse)
    }
}