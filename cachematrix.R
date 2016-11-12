## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Esta función crea un objeto matriz especial que puede almacenar en 
#caché su inversa.



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

#Esta función calcula la inversa de la matriz especial devuelta por 
#makeCacheMatrix arriba. Si ya se ha calculado el inverso (y la matriz 
#no ha cambiado), cacheSolve debería recuperar el inverso del caché


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