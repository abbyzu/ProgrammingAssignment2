## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Esta funci�n crea un objeto matriz especial que puede almacenar en 
#cach� su inversa.



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


## Write a short comment describing this function

#Esta funci�n calcula la inversa de la matriz especial devuelta por 
#makeCacheMatrix arriba. Si ya se ha calculado el inverso (y la matriz 
#no ha cambiado), cacheSolve deber�a recuperar el inverso del cach�


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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