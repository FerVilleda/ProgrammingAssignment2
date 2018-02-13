## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) {
  ##la inversa es nula al iniciar la funcion
  inv <- NULL
  ##Envia el valor y como x para la matriz
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##obtiene el valor de la matriz
  get <- function() x
  ##Obtiene y envia la inversa de la matriz
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ##elementos que se pueden consultar al utilizar la funcion
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Se revisa si hay valor de inversa si no se calcula
  inv <- x$getinv()
  if(!is.null(inv)){
    ##Se regresan los datos almacenados
    message("Obteniendo los datos almacenados")
    return(inv)
  }
  ##Se calcula la inversa
  mat.data = x$get()
  inv = solve(mat.data, ...)
  ##Se envia para almacenar en cache
  x$setinv(inv)
  ##Se regresa el valor calculado
  return(inv)
}







