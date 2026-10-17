# MEDIA MÓVIL CENTRADA
media_movil_centrada <- function(y, k) {
  
  stopifnot(is.numeric(y))
  stopifnot(length(k) == 1, k >= 2, k == as.integer(k))
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (k >= T) {
    stop("La ventana k debe ser menor que la longitud de la serie.")
  }
  
  resultado <- rep(NA_real_, T)
  
  # Si k es impar
  if (k %% 2 == 1) {
    
    mitad <- (k - 1) / 2
    
    for (t in (mitad + 1):(T - mitad)) {
      resultado[t] <- mean(
        y[(t - mitad):(t + mitad)]
      )
    }
    
  } else {
    
    # Si k es par
    mm <- rep(NA_real_, T)
    
    for (t in k:T) {
      mm[t] <- mean(y[(t - k + 1):t])
    }
    
    for (t in (k / 2 + 1):(T - k / 2)) {
      resultado[t] <- mean(
        mm[t - k / 2 + 1],
        mm[t - k / 2 + 2]
      )
    }
  }
  
  return(resultado)
}
