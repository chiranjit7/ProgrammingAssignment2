# this is to calculate the matrix functions which cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){
    x
  } 
  setmatrix<-function(inverse){ #Feel comfortable in building functions in this way
    m <<- inverse
  } 
  getmatrix<-function(){
    m
  }  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m) & !is.character(m)) { #m can be character in my code, so you see an aditional command here
    message("getting cached data")
    return(m)
  }
  matrix <-x$get() # I feel it is important to have the error printing for this code
  if (is.character(try(solve(matrix, ...),T))==T){
      m <- x$setmatrix("Most probably your matrix is either not a square matrix or it is not invertible; get the details from http://www.netlib.org/lapack") 
    } else {
      m <-solve(matrix, ...)
      x$setmatrix(m)     
    }
  m
}
