#This code will first cache the inverse of an inputted SQUARE matrix. then call it.
#first will calculate and cache
makeCacheMatrix<-function(x = matrix()) {
  #set some empty values and vectors to be populated later
  m <- NULL
  set<-function(y) {
    x<<-y
    m<<- NULL
  }
  get<-function() x
  #solve is the function to obtain the inverse of the matrix here
  #x is the matrix and m is now the inverse of it
  setInv<-function(solve) m<<-solve
  getInv<-function() m
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}
cacheSolve<-function(x, ...){
  m<-x$getInv()
  #if inverse was calculated in makeCacheMatrix then it is not null
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInv(m)
  m
}
#run the following two lines uncommented where x is the matrix
#CM<-makeCacheMatrix(x)
#cacheSolve(CM)