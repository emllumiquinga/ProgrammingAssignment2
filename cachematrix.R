#Erika Llumiquinga

# Este programa esta diseñado para calcular la inversa de una matriz, 
#y almacenar el resultado en la memoria caché, con el objetivo de volver 
#a ver el resultado, solo llamando a la funcion creada.Tambien ahorramos 
#tiempo al calcular la inversa si los datos de la matriz se han modificado.

# makeCacheMatrix()
#datos de entrada: x, es la matriz 
#datos de salida: las funciones (set,get,setinv,getinv)
#Esta función crea una matriz, que se puede almacenar en caché su inversa
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #set: establece el valor de la matriz
    x <<- y
    inv <<- NULL
  }
  get <- function() x   #get: devuelve los datos de la matriz x
  setinv <- function(inverse) inv <<- inverse   #setinv:establece la inversa de la matriz x
  getinv <- function() inv  #getinv:devuelve la inversa de la matriz x
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
}



# cacheSolve()
#datos de entrada: x, la matriz a la cual se calculará su inversa
#datos de salida: la inversa de x
# Si la inversa ya se ha calculado (y la matriz no ha cambiado), 
#entonces la funcion  debe recuperar la inversa de la caché.
cacheSolve <- function(x, ...) {
        ## Nos da la inversa de "x"
inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # si la matriz no ha cambiado, nos da la inversa ya calculada y almacenada en cache
  }
  #si la matriz ha cambiado, calcula y nos da la nueva inversa
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}


