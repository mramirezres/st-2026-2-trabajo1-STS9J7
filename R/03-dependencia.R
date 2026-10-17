#ACF MUESTRAL
acf_muestral <- function(y, m = NULL) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  n <- length(y)
  
  if (n < 2) {
    stop("La serie debe tener al menos dos observaciones.")
  }
  
  if (is.null(m)) {
    m <- min(floor(n / 4), 24)
  }
  
  if (m < 1 || m >= n) {
    stop("El número de rezagos debe ser mayor que 0 y menor que n.")
  }
  
  media <- mean(y)
  
  divisor <- sum((y - media)^2)
  
  r <- numeric(m)
  
  for (h in 1:m) {
    
    numerador <- sum(
      (y[(h + 1):n] - media) *
        (y[1:(n - h)] - media)
    )
    
    r[h] <- numerador / divisor
  }
  
  return(r)
}