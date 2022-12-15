## The pair of functions in this program cache the inverse of a square matrix

## The first function that I've written is called makeCacheMatrix. This functions creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
                }
        get<-function() x
        setInverseofMatrix<-function(inverse) inv<<-inverse
        getInverseofMatrix<-function() inv
        list(set=set,get=get,
             setInverseofMatrix=setInverseofMatrix,
             getInverseofMatrix=getInverseofMatrix)
}        
        
## The second function that I have written is called cacheSolve. This function computes the inverse of the special matrix returned by the makeCacheMatrix function that I have written above. If the inverse has already been calculated and the matrix has not changed, then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        inv<-x$getInverseofMatrix()
        if(!is.null(inv)){
                message("Fetching the data that has been cached")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data,...)
        x$setInverseofMatrix(inv)
        inv
}
