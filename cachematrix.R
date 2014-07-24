## Funcion que muestra el resultado de la inversa de una matriz.
## Revisa si esta almacenado el resultado, si esta almacenado lo despliega y si no esta almacenado lo calcula.

## Guarda el calculo de la inversa de la matriz

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
        set <- function(y) {
                x <<- solve(y)
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)


}

## Esta funcion revisa si esta guardada la operación
## Si no esta guardada la calcula y si esta guardad
## unicamente despliega el resultado

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m


}
