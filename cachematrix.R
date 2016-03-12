## Script contains two functions that allow for cached computation of a matrix inverse.


## makeCacheMatrix takes an invertible square matrix as input and constructs an object 
##that can cache it's inverse (once this is computed)


makeCacheMatrix <- function(x = matrix()) {
    InverseMatrix <- NULL
    GetCurrentMatrix <- function() x
    SetCurrentMatrix <- function(y){
        x <<- y
        InverseMatrix <<- NULL
    }
    SetInverseMatrix <- function(InverseValue) InverseMatrix <<- InverseValue
    GetInverseMatrix <- function() InverseMatrix
    list(GetCurrentMatrix = GetCurrentMatrix,SetCurrentMatrix = SetCurrentMatrix,
         SetInverseMatrix = SetInverseMatrix,GetInverseMatrix = GetInverseMatrix)
}


## cacheSolve takes an instance of makeCacheMatrix as input. It computes the inverse of the matrix  
## used to instantiate makeCacheMatrix, and stores this value. The next time it is called on the same 
## object it retrieves the cached value

cacheSolve <- function(x, ...) {
    InverseMatrix <- x$GetInverseMatrix()
    if (!is.null(InverseMatrix)) {
        message("I've seen this one, returning cached data...")
        return(InverseMatrix)
    }
    ToInvert <- x$GetCurrentMatrix()
    InverseMatrix <- solve(ToInvert)
    x$SetInverseMatrix(InverseMatrix)
    InverseMatrix
    ## Return a matrix that is the inverse of 'x'
}

