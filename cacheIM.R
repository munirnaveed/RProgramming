#Munir Naveed
#code is a modification of the example given on R programming course 
#for Assignment 2
# I made a few changes, e.g. re-name set, get, replace mean with solve. 
#however, there is one check on solve as well, becuaseI got some errors with simple tweak
# if we do not put checks, the program was giving error me on matrix(1:9, 3, 3)
# and matrix(1:10, 2, 5)
# I read the assumption on assignment page after modifying the code-- assumption
# that matrix is always invertible. anyway, it was fun to do more programming 

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv= getinv)
}
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
                     
        data <- x$get()
       #get dimensions of Matrix
        d<-dim(data)
        mDet<-det(data) 
        if(d[1]==d[2] & mDet!=0) # if it is a square matrx and not a singular then calculate inverse 
         {
            m <- solve(data, ...)
            x$setinv(m)
            return(m)
        }
        else
         {     if(mDet==0){
                             message("Matrix must a non Singular one")

                           }
                           else
                            {
                              message("Matrix must be Square")
                            }   
          } 
}