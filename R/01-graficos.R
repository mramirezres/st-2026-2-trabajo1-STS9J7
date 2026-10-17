# FUNCIÓN PARA GRAFICAR UNA SERIE DE TIEMPO
graficar_serie <- function(datos, titulo) {
  
  ggplot2::ggplot(datos, ggplot2::aes(x = fecha, y = y)) +
    
    ggplot2::geom_line(color = "purple") +
    
    ggplot2::labs(
      title = titulo,
      x = "Fecha",
      y = paste0("Valor (", attr(datos, "unidad"), ")"),
      caption = paste0(
        "Fuente: ", attr(datos, "fuente"),
        " | Observaciones: ", length(datos$y)
      )
    ) +
    
    ggplot2::theme_minimal()
}

# FUNCIÓN PARA GRAFICAR LA ESTACIONALIDAD
grafico_estacional <- function(datos, periodo) {
  
  # Identificar el ciclo y el período dentro del ciclo
  datos$ciclo <- ceiling(datos$t / periodo)
  datos$periodo <- (datos$t - 1) %% periodo + 1
  
  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = periodo, y = y, group = ciclo, color = factor(ciclo))
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Gráfico estacional",
      x = "Período",
      y = paste0("Valor (", attr(datos, "unidad"), ")"),
      color = "Ciclo"
    ) +
    ggplot2::theme_minimal()
}


# FUNCIÓN PARA GRAFICAR LOS REZAGOS
grafico_rezagos <- function(datos, rezago) {
  
  # Crear los valores actuales y los valores rezagados
  x <- datos$y[(rezago + 1):length(datos$y)]
  y <- datos$y[1:(length(datos$y) - rezago)]
  
  datos_rezago <- data.frame(
    rezagado = y,
    actual = x
  )
  
  ggplot2::ggplot(
    datos_rezago,
    ggplot2::aes(x = rezagado, y = actual)
  ) +
    ggplot2::geom_point(color = "purple") +
    ggplot2::labs(
      title = paste("Gráfico de rezago", rezago),
      x = paste("Y(t-", rezago, ")"),
      y = "Y(t)"
    ) +
    ggplot2::theme_minimal()
}
