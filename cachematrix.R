## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inicializo i
  i <- NULL
  ## Armo la matriz
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## muestro la matriz
  get <- function() {
    m
  }
  ## armar la función inversa de la matriz
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## muestro la matriz inversa
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calcule la inversa de la matriz especial devuelta por "makeCacheMatrix".
## En caso de no haberla calculado, entonces devuelve la inversa del caché
cacheSolve <- function(x, ...) {
  ## devuelve una matriz inversa de 'x'
  m <- x$getInverse()
  ## devuelve la matriz si no es nula
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## obtener la matriz de nuestro objeto
  data <- x$get()
  ## Calcular la inversa usando la multiplicación matricial
  m <- solve(data) %*% data
  ## Buscar la inversa del objeto
  x$setInverse(m)
  ## Mostramos la matriz
  m
}
