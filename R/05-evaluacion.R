# OPTIMIZACIÓN DE CONSTANTES Y VENTANAS

optimizar <- function(y, metodo, rejilla, s = NULL) {
  
  if (!is.numeric(y)) {
    stop("La serie debe ser numérica.")
  }
  
  if (!is.character(metodo)) {
    stop("El método debe ser un texto.")
  }
  
  if (any(is.na(y))) {
    stop("La serie contiene valores faltantes.")
  }
  
  if (length(y) < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  
  # MEDIA MÓVIL
  
  if (metodo == "mm") {
    
    resultados <- data.frame(
      k = rejilla,
      MSE = NA_real_
    )
    
    for (i in seq_along(rejilla)) {
      
      k <- rejilla[i]
      
      if (k < length(y)) {
        
        modelo <- ajustar_mm(y, k)
        
        posiciones <- which(!is.na(modelo$yhat))
        errores <- y[posiciones] - modelo$yhat[posiciones]
        
        resultados$MSE[i] <- mean(errores^2)
      }
    }
  }
  
  
  # DOBLE MEDIA MÓVIL
  
  else if (metodo == "dmm") {
    
    resultados <- data.frame(
      k = rejilla,
      MSE = NA_real_
    )
    
    for (i in seq_along(rejilla)) {
      
      k <- rejilla[i]
      
      if (2 * k - 1 < length(y)) {
        
        modelo <- ajustar_dmm(y, k)
        
        posiciones <- which(!is.na(modelo$yhat))
        errores <- y[posiciones] - modelo$yhat[posiciones]
        
        resultados$MSE[i] <- mean(errores^2)
      }
    }
  }
  
  
  # SUAVIZAMIENTO EXPONENCIAL SIMPLE
  
  else if (metodo == "ses") {
    
    resultados <- data.frame(
      alpha = rejilla,
      MSE = NA_real_
    )
    
    for (i in seq_along(rejilla)) {
      
      alpha <- rejilla[i]
      
      modelo <- ajustar_ses(y, alpha)
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  # HOLT LINEAL
  
  else if (metodo == "holt") {
    
    resultados <- expand.grid(
      alpha = rejilla,
      beta = rejilla
    )
    
    resultados$MSE <- NA_real_
    
    for (i in 1:nrow(resultados)) {
      
      modelo <- ajustar_holt(
        y,
        resultados$alpha[i],
        resultados$beta[i]
      )
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  # HOLT-WINTERS ADITIVO
  
  else if (metodo == "hw") {
    
    if (is.null(s)) {
      stop("Se debe indicar s para Holt-Winters.")
    }
    
    resultados <- expand.grid(
      alpha = rejilla,
      beta = rejilla,
      gamma = rejilla
    )
    
    resultados$MSE <- NA_real_
    
    for (i in 1:nrow(resultados)) {
      
      modelo <- ajustar_hw(
        y,
        s,
        resultados$alpha[i],
        resultados$beta[i],
        resultados$gamma[i]
      )
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  # HOLT-WINTERS MULTIPLICATIVO
  
  else if (metodo == "hw_mult") {
    
    if (is.null(s)) {
      stop("Se debe indicar s para Holt-Winters.")
    }
    
    resultados <- expand.grid(
      alpha = rejilla,
      beta = rejilla,
      gamma = rejilla
    )
    
    resultados$MSE <- NA_real_
    
    for (i in 1:nrow(resultados)) {
      
      modelo <- ajustar_hw_mult(
        y,
        s,
        resultados$alpha[i],
        resultados$beta[i],
        resultados$gamma[i]
      )
      
      posiciones <- which(!is.na(modelo$yhat))
      errores <- y[posiciones] - modelo$yhat[posiciones]
      
      resultados$MSE[i] <- mean(errores^2)
    }
  }
  
  
  else {
    stop("Método no reconocido.")
  }
  
  if (all(is.na(resultados$MSE))) {
    stop("Ningún parámetro de la rejilla es válido para esta serie.")
  }
  
  posicion <- which.min(resultados$MSE)
  
  optimo <- resultados[posicion, ]
  
  
  return(
    list(
      metodo = metodo,
      rejilla = resultados,
      optimo = optimo
    )
  )
}


# ORIGEN MOVIL
origen_movil <- function(y, fn_ajuste, H, n_eval, s, ...) {
  
  stopifnot(is.numeric(y))
  stopifnot(is.function(fn_ajuste))
  stopifnot(length(H) == 1, H >= 1, H == as.integer(H))
  stopifnot(length(n_eval) == 1, n_eval >= 1, n_eval == as.integer(n_eval))
  stopifnot(length(s) == 1, s >= 1, s == as.integer(s))
  
  if (any(is.na(y))) {
    stop("La serie y contiene valores faltantes.")
  }
  
  T <- length(y)
  
  if (n_eval + H >= T) {
    stop("n_eval + H debe ser menor que la longitud de la serie.")
  }
  
  
  # Argumentos del método
  args_extra <- list(...)
  
  if ("s" %in% names(formals(fn_ajuste))) {
    args_extra$s <- s
  }
  
  
  # Orígenes
  origenes <- (T - H - n_eval + 1):(T - H)
  
  
  # Pronósticos en h = 1 y h = H
  pred <- t(sapply(origenes, function(t) {
    mod_t <- do.call(fn_ajuste, c(list(y[1:t]), args_extra))
    p <- mod_t$pronosticar(H)[c(1, H)]
    
    return(p)
  }))
  
  
  
  # Métricas
  res_h1 <- medidas(y[origenes + 1], pred[, 1], y_estimacion = y[1:(T-H)], s = s)
  res_hH <- medidas(y[origenes + H], pred[, 2], y_estimacion = y[1:(T-H)], s = s)
  
  # Errores
  e_h1 <- y[origenes + 1] - pred[, 1]
  e_hH <- y[origenes + H] - pred[, 2]
  
  return(
    list(
      h1 = data.frame(RMSE = sqrt(res_h1$MSE), MAD = res_h1$MAD, MAPE = res_h1$MAPE),
      hH = data.frame(RMSE = sqrt(res_hH$MSE), MAD = res_hH$MAD, MAPE = res_hH$MAPE),
      pred_h1 = pred[, 1],  
      pred_hH = pred[, 2],
      e_h1 = e_h1,  
      e_hH = e_hH 
    )
  )
  
}



# MEDIDAS DE ERROR
medidas <- function(y, yhat, y_estimacion, s = 1) {
  
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(yhat))
  stopifnot(is.numeric(y_estimacion))
  
  if (length(s) != 1 || is.na(s) ||
      s < 1 || s != as.integer(s)) {
    stop("s debe ser un entero mayor o igual que 1.")
  }
  
  if (any(is.na(y)) || any(is.na(yhat)) ||
      any(is.na(y_estimacion))) {
    stop("No se permiten valores faltantes.")
  }
  
  if (length(y) != length(yhat)) {
    stop("y y yhat deben tener la misma longitud.")
  }
  
  if (length(y) < 1) {
    stop("Se necesita al menos una observación para evaluar.")
  }
  
  T_est <- length(y_estimacion)
  
  if (s >= T_est) {
    stop("s debe ser menor que la longitud del tramo de estimación.")
  }
  
  # Error de pronóstico
  e <- y - yhat
  
  # MSE
  MSE <- mean(e^2)
  
  # RMSE
  RMSE <- sqrt(MSE)
  
  # MAD
  MAD <- mean(abs(e))
  
  # MAPE
  if (any(y == 0)) {
    MAPE <- NA_real_
  } else {
    MAPE <- 100 * mean(abs(e / y))
  }
  
  # MASE: escala calculada en el tramo de estimación
  error_ingenuo <- abs(
    y_estimacion[(s + 1):T_est] -
      y_estimacion[1:(T_est - s)]
  )
  
  escala <- mean(error_ingenuo)
  
  if (escala == 0) {
    MASE <- NA_real_
  } else {
    MASE <- MAD / escala
  }
  
  return(
    list(
      MSE = MSE,
      RMSE = RMSE,
      MAD = MAD,
      MAPE = MAPE,
      MASE = MASE
    )
  )
}

# FUNCIÓN JARQUE BERA
jarque_bera <- function(e) {
  
  stopifnot(is.numeric(e))
  
  if (any(is.na(e))) {
    stop("Los errores contienen valores faltantes.")
  }
  
  N <- length(e)
  
  if (N < 3) {
    stop("Se necesitan al menos 3 observaciones.")
  }
  
  media <- mean(e)
  
  # Calcular los momentos centrales
  momento_2 <- mean((e - media)^2)
  momento_3 <- mean((e - media)^3)
  momento_4 <- mean((e - media)^4)
  
  # Calcular asimetría
  A <- momento_3 / momento_2^(3/2)
  
  # Calcular curtosis
  K <- momento_4 / momento_2^2
  
  # Calcular el estadístico Jarque-Bera
  JB <- N / 6 * (A^2 + (K - 3)^2 / 4)
  
  # Grados de libertad
  gl <- 2
  
  # Valor crítico al 5 %
  valor_critico <- stats::qchisq(0.95, df = gl)
  
  # Valor p
  valor_p <- stats::pchisq(
    JB,
    df = gl,
    lower.tail = FALSE
  )
  
  return(
    list(
      estadistico = JB,
      grados_libertad = gl,
      valor_critico = valor_critico,
      valor_p = valor_p
    )
  )
}


# FUNCIÓN DURBIN WATSON
durbin_watson <- function(e, k_regresores = 1) {
  
  stopifnot(is.numeric(e))
  
  if (any(is.na(e))) {
    stop("Los errores contienen valores faltantes.")
  }
  
  N <- length(e)
  
  if (N < 2) {
    stop("Se necesitan al menos 2 observaciones.")
  }
  
  if (sum(e^2) == 0) {
    stop("El denominador es cero.")
  }
  
  # Estadístico de prueba
  d <- sum((e[2:N] - e[1:(N - 1)])^2) / sum(e^2)
  
  # Aproximación con el primer coeficiente de autocorrelación r1
  r1 <- acf(e, lag.max = 2, plot = FALSE)$acf[2]
  d_aprox <- 2 * (1 - r1)
  
  # Cotas críticas d_L y d_U (Aproximación teórica para alpha = 0.05)
  z_alpha <- 1.96
  dL <- max(0.8, 2 - z_alpha * sqrt(4 / N) - (1.5 * k_regresores / N))
  dU <- min(3.2, 2 - z_alpha * sqrt(4 / N) + (3.5 * k_regresores / N))
  
  return(
    list(
      estadistico = d,
      d_aprox     = d_aprox,
      dL          = dL,
      dU          = dU
    )
  )
}


# VALIDAR ERRORES
validar_errores <- function(e, m = NULL, p = 0) {
  
  if (!is.numeric(e) || any(!is.finite(e))) {
    stop("Los errores deben ser numéricos y finitos.")
  }
  
  n <- length(e)
  
  if (n < 3) {
    stop("Se necesitan al menos tres errores.")
  }
  
  if (length(p) != 1 || !is.numeric(p) ||
      !is.finite(p) || p < 0 || p != as.integer(p)) {
    stop("p debe ser un entero mayor o igual que 0.")
  }
  
  if (is.null(m)) {
    m <- min(floor(n / 4), 24)
  }
  
  if (length(m) != 1 || !is.numeric(m) ||
      !is.finite(m) || m < 1 ||
      m != as.integer(m) || m >= n || p >= m) {
    stop("m debe ser un entero entre 1 y n - 1, y mayor que p.")
  }
  
  # GRÁFICO DE LOS ERRORES
  
  datos_errores <- data.frame(
    t = 1:n,
    error = e
  )
  
  grafico_errores <- ggplot2::ggplot(
    datos_errores,
    ggplot2::aes(x = t, y = error)
  ) +
    ggplot2::geom_line(color = "purple") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(
      title = "Errores de pronóstico",
      x = "Tiempo",
      y = "Error"
    ) +
    ggplot2::theme_minimal()
  
  # CORRELOGRAMA DE LOS ERRORES
  
  correlograma_errores <- correlograma(e, m)
  
  # PRUEBA T DE MEDIA CERO
  
  media <- mean(e)
  desviacion <- sd(e)
  
  if (desviacion == 0) {
    estadistico_t <- NA_real_
    valor_p_t <- NA_real_
  } else {
    estadistico_t <- media / (desviacion / sqrt(n))
    valor_p_t <- 2 * pt(
      -abs(estadistico_t),
      df = n - 1
    )
  }
  
  grados_libertad_t <- n - 1
  
  # LJUNG-BOX
  
  resultado_ljung <- ljung_box(
    r = correlograma_errores$acf,
    T = n,
    m = m,
    p = p
  )
  
  # JARQUE-BERA
  
  resultado_jb <- jarque_bera(e)
  
  # DURBIN-WATSON
  
  resultado_dw <- durbin_watson(e)
  
  return(list(
    errores = e,
    grafico_errores = grafico_errores,
    correlograma = correlograma_errores,
    media = media,
    t = estadistico_t,
    grados_libertad_t = grados_libertad_t,
    valor_p_t = valor_p_t,
    ljung_box = resultado_ljung,
    jarque_bera = resultado_jb,
    durbin_watson = resultado_dw
  ))
}




