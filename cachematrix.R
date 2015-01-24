## Matrix inversion is a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. The below two functions are used to create a special matrix that can store a matrix and cache the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL    
    }
    get<-function()x
    setinverse<-function(solve) inv<<-solve
    getinverse<-function()inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv<-x$getinverse()
        ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)){
        message("get the cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinverse(inv)
    inv
}
x<-matrix(1:4,2,2)
cach<-makeCacheMatrix(x)
cach$getinverse()
i<-solve(x)
cach$setinverse(i)
cach$getinverse()


