## Inverse of a Matrix shall be created using this

## creates a special matrix which really is a list containing function set, setmatrix, get, getmatrix 

makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
        
}


## the following function calcuates the inverse of matrix. it first checks to see if the inverse has already been calculated. if it has been calculated, then it skips computation and returns the result from cache. otherwise it performs the calculation

cacheSolve <- function(x, ...) {
        ## this will Return a matrix that is the inverse of 'x'
        
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
        
}