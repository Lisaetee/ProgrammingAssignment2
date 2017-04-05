## first function creates x as the matrix set to other environment

MakeMatrix <- function(x = matrix()) {
      inv = NULL 
      set = function() y 
            x <<- y  ##environment global
            inv <<- NULL  ##environment global

      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## solve the function. If inverse return, if not inverse, solve and return
cachesolve <-function(x, ...) {
      inv = x$getinverse()
      if(!is.null(m)) {  ##check if inverse, if so, return
            message("getting cached data")
            return(m)
      }
      data = x$get()
      inv = solve(data, ...)  #if not inverse, solve and return
      x$setinv(inv)
      return(inv)
}

##test file
##
mat1 <--(x)
> show(x)
[,1] [,2] [,3]
[1,]    2    4    6
[2,]    1    3    5
> show(mat1)
[,1] [,2] [,3]
[1,]   -2   -4   -6
[2,]   -1   -3   -5
