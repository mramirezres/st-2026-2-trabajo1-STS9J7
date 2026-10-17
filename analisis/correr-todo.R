#CARGAR LAS FUNCIONES
source("R/00-lectura.R")
source("R/01-graficos.R")
source("R/02-operadores.R")
source("R/03-dependencia.R")
source("R/04-metodos.R")
source("R/05-evaluacion.R")

# GUARDAR GRAFICOS 
if (!dir.exists("figs")) {
dir.create("figs")
}


# VERIFICACION diferenciar()
y <- 1:10

resultado_propio <- diferenciar(y, d = 1)
resultado_R <- diff(y)

all.equal(resultado_propio, resultado_R)


# Verificación de diferencia estacional

y <- 1:15

resultado_propio <- diferenciar(y, D = 1, s = 3)
resultado_R <- y[4:15] - y[1:12]

all.equal(resultado_propio, resultado_R)


#Verificación de polinomio_rezago()
# Polinomio: 1 - 3B + 2B^2
resultado_raices <- polinomio_rezago(c(-3, 2))
stopifnot(
  isTRUE(all.equal(
    sort(resultado_raices$modulo),
    c(0.5, 1)
  ))
)

cat("La verificación de polinomio_rezago() fue exitosa.\n")



# Verificación de pacf_dl contra pacf() de R
y_prueba <- c(
  10, 12, 13, 15, 14, 16, 18, 17, 19, 21,
  20, 22, 24, 23, 25, 27, 26, 28, 30, 29
)

m <- 8

# Autocorrelaciones muestrales hasta el rezago m
r <- acf(y_prueba, lag.max = m, plot = FALSE)$acf[-1]

# PACF calculada con nuestra función
pacf_propia <- pacf_dl(as.numeric(r))

# PACF calculada con la función de R
pacf_R <- as.numeric(pacf(y_prueba, lag.max = m, plot = FALSE)$acf)

# Diferencia absoluta máxima
diferencia_maxima <- max(abs(pacf_propia - pacf_R))

diferencia_maxima
diferencia_maxima < 1e-10


#VERIFIACIONES DE LOS MÉTODOS

#MEDIA SIMPLE
y <- c(10, 12, 14, 16, 18)
modelo <- ajustar_media(y)
modelo$yhat
modelo$pronosticar(3)

# INGENUO
y <- c(10, 12, 14, 16, 18)
modelo <- ajustar_ingenuo(y)
modelo$yhat
modelo$pronosticar(3)

# INGENUO ESTACIONAL
y <- c(10, 20, 30, 40,
       12, 22, 32, 42)
modelo <- ajustar_ingenuo_estacional(y, s = 4)
modelo$yhat
modelo$pronosticar(4)

# MEDIA MÓVIL
y <- c(18.43, 19.98, 19.51, 20.63,
       19.78, 21.25, 21.28, 22.14)
resultado <- ajustar_mm(y, k = 3)
resultado$yhat
resultado$pronosticar(3)

# SUAVIZAMIENTO EXPONENCIAL SIMPLE
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_ses(y, alpha = 0.3)
resultado$yhat
resultado$pronosticar(3)

# DOBLE MEDIA MÓVIL
y <- c(133, 155, 165, 171, 194, 231, 274, 312, 313, 333, 343)
resultado <- ajustar_dmm(y, k = 3)
resultado$yhat
resultado$pronosticar(3)

# TENDENCIA LINEAL
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_tendencia(y, tipo = "lineal")
resultado$yhat
resultado$pronosticar
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
modelo_lm <- lm(y ~ t)

coef(modelo_lm)

# TENDENCIA CUADRÁTICA
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_tendencia(y, tipo = "cuadratica")
resultado$yhat
resultado$pronosticar(3)
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
modelo_lm <- lm(y ~ t + I(t^2))
coef(modelo_lm)

# TENDENCIA EXPONENCIAL
y <- c(10, 12, 13, 15, 16, 18, 20, 21)
resultado <- ajustar_tendencia(y, tipo = "exponencial")
resultado$yhat
resultado$pronosticar(3)
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
modelo_lm <- lm(log(y) ~ t)
coef(modelo_lm)

# TENDENCIA CON FICTICIAS
y <- c(10, 14, 12, 16,
       12, 16, 14, 18)
resultado <- ajustar_tendencia_estacional(y, s = 4)
resultado$yhat
resultado$pronosticar(4)
resultado$parametros$coeficientes

# Comparación con lm()
t <- 1:length(y)
temporada <- (t - 1) %% 4 + 1

f1 <- ifelse(temporada == 1, 1, 0) -
  ifelse(temporada == 4, 1, 0)

f2 <- ifelse(temporada == 2, 1, 0) -
  ifelse(temporada == 4, 1, 0)

f3 <- ifelse(temporada == 3, 1, 0) -
  ifelse(temporada == 4, 1, 0)

modelo_lm <- lm(y ~ t + f1 + f2 + f3)

coef(modelo_lm)

# HOLT
y <- c(10, 12, 15, 14, 18, 20)
resultado <- ajustar_holt(y, alpha = 0.3, beta = 0.2)
resultado$yhat
resultado$pronosticar(3)

# HOLT-WINTERS ADITIVO
y <- c(10, 12, 14, 16,
       12, 14, 16, 18)
resultado <- ajustar_hw(y, s = 4,
                        alpha = 0.3,
                        beta = 0.2,
                        gamma = 0.4)
resultado$yhat
resultado$pronosticar(4)

# HOLT-WINTERS MULTIPLICATIVO
y <- c(10, 12, 14, 16,
       12, 14, 16, 18)

resultado <- ajustar_hw_mult(y, s = 4,
                             alpha = 0.3,
                             beta = 0.2,
                             gamma = 0.4)
resultado$yhat
resultado$pronosticar(4)


#Cargar las librerias
library(ggplot2)
library(patchwork)
library(dplyr)
library(tibble)
library(knitr)

#Cargar serie 1
serie1 <- leer_serie(
  "datos/STS9J7-serie-1.csv",
  "Almacenes La Ceiba",
  "miles de pesos"
)

cat("Nombre: Valor promedio semanal de la compra\n",
    "Unidad:", attr(serie1, "unidad"), "\n",
    "Frecuencia:", attr(serie1, "frecuencia"), "\n",
    "Fecha inicial:", as.character(min(serie1$fecha)), "\n",
    "Fecha final:", as.character(max(serie1$fecha)), "\n",
    "T:", length(serie1$y), "\n",
    "Mínimo:", min(serie1$y), "\n",
    "Máximo:", max(serie1$y), "\n",
    "Media:", mean(serie1$y), "\n"
)


# Verificar la ficha de la serie 1
stopifnot(
  length(serie1$y) == 200,
  min(serie1$y) == 451.8,
  max(serie1$y) == 577.5,
  isTRUE(all.equal(mean(serie1$y), 500.996)),
  as.character(min(serie1$fecha)) == "2022-03-07",
  as.character(max(serie1$fecha)) == "2025-12-29",
  attr(serie1, "frecuencia") == "semanal",
  attr(serie1, "unidad") == "miles de pesos"
)

cat("La ficha de la serie 1 fue verificada correctamente.\n")

#cargar serie 2
serie2 <- leer_serie(
  "datos/STS9J7-serie-2.csv",
  "Almacenes La Ceiba",
  "cientos de clientes"
)

cat("Nombre: Clientes registrados semanalmente\n",
    "Unidad:", attr(serie2, "unidad"), "\n",
    "Frecuencia:", attr(serie2, "frecuencia"), "\n",
    "Fecha inicial:", as.character(min(serie2$fecha)), "\n",
    "Fecha final:", as.character(max(serie2$fecha)), "\n",
    "T:", length(serie2$y), "\n",
    "Mínimo:", min(serie2$y), "\n",
    "Máximo:", max(serie2$y), "\n",
    "Media:", mean(serie2$y), "\n"
)

# Verificar la ficha de la serie 2
stopifnot(
  length(serie2$y) == 180,
  isTRUE(all.equal(min(serie2$y), 300)),
  isTRUE(all.equal(max(serie2$y), 440)),
  isTRUE(all.equal(mean(serie2$y), 369.2722, tolerance = 0.0001)),
  as.character(min(serie2$fecha)) == "2022-07-04",
  as.character(max(serie2$fecha)) == "2025-12-08",
  attr(serie2, "frecuencia") == "semanal",
  attr(serie2, "unidad") == "cientos de clientes"
)

cat("La ficha de la serie 2 fue verificada correctamente.\n")

#cargar serie 3
serie3 <- leer_serie(
  "datos/STS9J7-serie-3.csv",
  "Almacenes La Ceiba",
  "millones de pesos"
)

cat("Nombre: Ventas diarias del supermercado\n",
    "Unidad:", attr(serie3, "unidad"), "\n",
    "Frecuencia:", attr(serie3, "frecuencia"), "\n",
    "Fecha inicial:", as.character(min(serie3$fecha)), "\n",
    "Fecha final:", as.character(max(serie3$fecha)), "\n",
    "T:", length(serie3$y), "\n",
    "Mínimo:", min(serie3$y), "\n",
    "Máximo:", max(serie3$y), "\n",
    "Media:", mean(serie3$y), "\n"
)

# Verificar la ficha de la serie 3
stopifnot(
  length(serie3$y) == 196,
  isTRUE(all.equal(min(serie3$y), 335.1)),
  isTRUE(all.equal(max(serie3$y), 499.2)),
  isTRUE(all.equal(mean(serie3$y), 406.7862, tolerance = 0.0001)),
  as.character(min(serie3$fecha)) == "2025-03-03",
  as.character(max(serie3$fecha)) == "2025-09-14",
  attr(serie3, "frecuencia") == "diaria",
  attr(serie3, "unidad") == "millones de pesos"
)

cat("La ficha de la serie 3 fue verificada correctamente.\n")

#particion de las series
# Calcular n0 para cada serie
n0_1 <- floor(0.8 * length(serie1$y))
n0_2 <- floor(0.8 * length(serie2$y))
n0_3 <- floor(0.8 * length(serie3$y))

# Crear tabla de partición
particion <- data.frame(
  Serie = c("Serie 1", "Serie 2", "Serie 3"),
  
  T = c(
    length(serie1$y),
    length(serie2$y),
    length(serie3$y)
  ),
  
  n0 = c(n0_1, n0_2, n0_3),
  
  H = c(4, 6, 7),
  
  origen_h1 = c(
    length(serie1$y) - n0_1,
    length(serie2$y) - n0_2,
    length(serie3$y) - n0_3
  ),
  
  origen_H = c(
    length(serie1$y) - n0_1 - 4 + 1,
    length(serie2$y) - n0_2 - 6 + 1,
    length(serie3$y) - n0_3 - 7 + 1
  ),
  
  inicio_estimacion = c(
    as.character(serie1$fecha[1]),
    as.character(serie2$fecha[1]),
    as.character(serie3$fecha[1])
  ),
  
  fin_estimacion = c(
    as.character(serie1$fecha[n0_1]),
    as.character(serie2$fecha[n0_2]),
    as.character(serie3$fecha[n0_3])
  ),
  
  primer_origen_h1 = c(
    as.character(serie1$fecha[n0_1]),
    as.character(serie2$fecha[n0_2]),
    as.character(serie3$fecha[n0_3])
  ),
  
  ultimo_origen_h1 = c(
    as.character(serie1$fecha[length(serie1$y) - 1]),
    as.character(serie2$fecha[length(serie2$y) - 1]),
    as.character(serie3$fecha[length(serie3$y) - 1])
  ),
  
  primer_origen_H = c(
    as.character(serie1$fecha[n0_1]),
    as.character(serie2$fecha[n0_2]),
    as.character(serie3$fecha[n0_3])
  ),
  
  ultimo_origen_H = c(
    as.character(serie1$fecha[length(serie1$y) - 4]),
    as.character(serie2$fecha[length(serie2$y) - 6]),
    as.character(serie3$fecha[length(serie3$y) - 7])
  ),
  
  inicio_validacion = c(
    as.character(serie1$fecha[n0_1 + 1]),
    as.character(serie2$fecha[n0_2 + 1]),
    as.character(serie3$fecha[n0_3 + 1])
  ),
  
  fin_validacion = c(
    as.character(serie1$fecha[length(serie1$y)]),
    as.character(serie2$fecha[length(serie2$y)]),
    as.character(serie3$fecha[length(serie3$y)])
  )
)

particion

#DESCRIPCION Y PATRON
# Serie 1
grafico_serie1 <- graficar_serie(
  serie1,
  "Valor promedio semanal de compras"
)

resultado_cor1 <- correlograma(serie1$y)

panel1 <- panel_diagnostico(
  grafico_serie1,
  resultado_cor1
)

print(panel1)

# Gráficos de rezagos de la Serie 1
p1 <- grafico_rezagos(serie1, 1)
p2 <- grafico_rezagos(serie1, 4)
p3 <- grafico_rezagos(serie1, 13)
p4 <- grafico_rezagos(serie1, 52)

(p1 | p2) / (p3 | p4)

resultado_cor52 <- correlograma(serie1$y, 52)
resultado_cor52$acf[c(1, 4, 13, 52)]

# Serie 2
grafico_serie2 <- graficar_serie(
  serie2,
  "Clientes registrados semanalmente"
)

resultado_cor2 <- correlograma(serie2$y)

panel2 <- panel_diagnostico(
  grafico_serie2,
  resultado_cor2
)

print(panel2)

# Gráficos de rezagos de la Serie 2
p1_2 <- grafico_rezagos(serie2, 1)
p2_2 <- grafico_rezagos(serie2, 4)
p3_2 <- grafico_rezagos(serie2, 13)
p4_2 <- grafico_rezagos(serie2, 52)

(p1_2 | p2_2) / (p3_2 | p4_2)

y_dif2 <- diferenciar(serie2$y, 1, 0, 1)
r_dif2 <- acf_muestral(y_dif2, 52)
banda2 <- 1.96 / sqrt(length(y_dif2))

round(r_dif2[c(4, 13, 26, 52)], 3)
banda2
sum(abs(r_dif2) > banda2)

# Serie 3
grafico_serie3 <- graficar_serie(
  serie3,
  "Ventas diarias del supermercado"
)

resultado_cor3 <- correlograma(serie3$y)

panel3 <- panel_diagnostico(
  grafico_serie3,
  resultado_cor3
)

print(panel3)

# Grafico de rezagos
p1_3 <- grafico_rezagos(serie3, 1)
p2_3 <- grafico_rezagos(serie3, 7)
p3_3 <- grafico_rezagos(serie3, 14)
p4_3 <- grafico_rezagos(serie3, 30)

(p1_3 | p2_3) / (p3_3 | p4_3)

grafico_estacional(serie3, 7)

#Grafico de amplitud vs nivel
semana <- ceiling(seq_along(serie3$y) / 7)

nivel    <- tapply(serie3$y, semana, mean)
amplitud <- tapply(serie3$y, semana, function(x) max(x) - min(x))

correlacion <- cor(nivel, amplitud)

datos <- data.frame(nivel = as.numeric(nivel), amplitud = as.numeric(amplitud))

ggplot(datos, aes(x = nivel, y = amplitud)) +
  geom_point(color = "purple", size = 2.5) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Amplitud vs. nivel medio de cada semana",
    subtitle = paste("Correlación =", round(correlacion, 3)),
    x = "Nivel medio de la semana (millones de pesos)",
    y = "Amplitud de la semana (millones de pesos)"
  ) +
  theme_minimal()

# Media móvil centrada de la serie 3
tendencia3 <- media_movil_centrada(serie3$y, 7)

head(tendencia3)

# Descomposición clásica de la serie 3
resultado3 <- descomposicion_clasica(
  y = serie3$y,
  periodo = 7,
  tipo = "aditiva"
)

resultado3$indices_estacionales

# Preparar los datos para los gráficos
datos3 <- data.frame(
  fecha = serie3$fecha,
  ventas = serie3$y,
  tendencia = resultado3$tendencia,
  residuo = resultado3$residuo
)

# Graficar la tendencia
ggplot(datos3, aes(x = fecha, y = tendencia)) +
  geom_line(color = "purple") +
  labs(
    title = "Tendencia estimada - Serie 3",
    x = "Fecha",
    y = "Ventas (millones de pesos)"
  ) +
  theme_minimal()

# Graficar los índices estacionales
indices3 <- data.frame(
  dia = 1:7,
  indice = resultado3$indices_estacionales
)

ggplot(indices3, aes(x = dia, y = indice)) +
  geom_line(color = "purple") +
  geom_point(color = "purple", size = 2.5) +
  scale_x_continuous(
    breaks = 1:7,
    labels = c("Lunes", "Martes", "Miércoles", "Jueves",
               "Viernes", "Sábado", "Domingo")
  ) +
  labs(
    title = "Índices estacionales - Serie 3",
    x = "Día de la semana",
    y = "Índice estacional (millones de pesos)"
  ) +
  theme_minimal()

# Graficar el residuo
ggplot(datos3, aes(x = fecha, y = residuo)) +
  geom_line(color = "purple") +
  labs(
    title = "Residuo de la descomposición - Serie 3",
    x = "Fecha",
    y = "Residuo"
  ) +
  theme_minimal()

# Polinomio de la primera diferencia
raices_serie1 <- polinomio_rezago(c(-1))

raices_serie1

# Segunda diferencia
raices_serie1_2 <- polinomio_rezago(c(-2, 1))
raices_serie1_2

# Definir los puntos del círculo unitario (radio = 1)
circulo <- data.frame(
  x = cos(seq(0, 2 * pi, length.out = 200)),
  y = sin(seq(0, 2 * pi, length.out = 200))
)

# Gráfico para la Candidata 1
g1 <- ggplot(raices_serie1, aes(x = Re(raiz), y = Im(raiz))) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_path(data = circulo, aes(x = x, y = y), color = "gray40", linetype = "dotted") +
  geom_point(color = "purple", size = 3) +
  annotate("text", x = 1, y = 0.15, label = "Raíz = 1", color = "purple") +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.2)) +
  labs(
    title = "Candidata 1: 1 - B",
    x = "Parte real",
    y = "Parte imaginaria"
  ) +
  theme_minimal()

# Gráfico para la Candidata 2
g2 <- ggplot(raices_serie1_2, aes(x = Re(raiz), y = Im(raiz))) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_path(data = circulo, aes(x = x, y = y), color = "gray40", linetype = "dotted") +
  geom_point(color = "purple", size = 3) +
  annotate("text", x = 0.85, y = 0.15, label = "Raíz doble = 1", color = "purple") +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.2)) +
  labs(
    title = "Candidata 2: (1 - B)²",
    x = "Parte real",
    y = "Parte imaginaria"
  ) +
  theme_minimal()

(g1 | g2) + 
  plot_annotation(
    title = "Plano complejo de raíces de los operadores - Serie 1",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Varianza muestral y autocorrelación de rezago 1

# Serie original y sus transformaciones
y1 <- serie1$y

y1_d1 <- diferenciar(y1, d = 1)
y1_d2 <- diferenciar(y1, d = 2)

# Tabla de varianzas y autocorrelaciones
tabla_serie1 <- data.frame(
  Transformacion = c(
    "Original",
    "Primera diferencia",
    "Segunda diferencia"
  ),
  Varianza = c(
    var(y1),
    var(y1_d1),
    var(y1_d2)
  ),
  r1 = c(
    acf_muestral(y1, 1)[1],
    acf_muestral(y1_d1, 1)[1],
    acf_muestral(y1_d2, 1)[1]
  )
)

tabla_serie1$Varianza <- round(tabla_serie1$Varianza, 4)
tabla_serie1$r1 <- round(tabla_serie1$r1, 4)

tabla_serie1


# Medias observadas
media_d1 <- mean(y1_d1)
media_d2 <- mean(y1_d2)

# Medias esperadas según las transformaciones
media_d1_esperada <- (y1[length(y1)] - y1[1]) /
  (length(y1) - 1)

media_d2_esperada <- (
  (y1[length(y1)] - y1[length(y1) - 1]) -
    (y1[2] - y1[1])
) / (length(y1) - 2)

# Comparación
data.frame(
  Transformacion = c("Primera diferencia", "Segunda diferencia"),
  Media_observada = c(media_d1, media_d2),
  Media_esperada = c(media_d1_esperada, media_d2_esperada)
)

stopifnot(
  isTRUE(all.equal(media_d1, media_d1_esperada)),
  isTRUE(all.equal(media_d2, media_d2_esperada))
)

cat("Las medias coinciden con lo esperado.\n")

# Varianza de la serie original
gamma0 <- var(y1)

# Autocovarianza muestral de rezago 1
gamma1 <- cov(y1[-length(y1)], y1[-1])

# Varianza de la primera diferencia según la identidad
varianza_identidad <- 2 * gamma0 - 2 * gamma1

# Varianza calculada directamente
varianza_directa <- var(y1_d1)

data.frame(
  Varianza_identidad = varianza_identidad,
  Varianza_directa = varianza_directa
)

# Candidata 1: primera diferencia de la serie 2
raices_serie2_1 <- polinomio_rezago(c(-1))

raices_serie2_1

# Candidata 2: segunda diferencia de la serie 2
raices_serie2_2 <- polinomio_rezago(c(-2, 1))

raices_serie2_2

# Unir las raíces de las dos candidatas
raices_serie2 <- rbind(
  raices_serie2_1,
  raices_serie2_2
)

# Definir los puntos del círculo unitario
circulo <- data.frame(
  x = cos(seq(0, 2 * pi, length.out = 200)),
  y = sin(seq(0, 2 * pi, length.out = 200))
)

# Graficar las raíces con el círculo unitario
ggplot(raices_serie2, aes(x = Re(raiz), y = Im(raiz))) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_path(data = circulo, aes(x = x, y = y), color = "gray40", linetype = "dotted") +
  geom_point(color = "purple", size = 3) +
  annotate("text", x = 0.7, y = 0.15, label = "Raíces en B = 1", color = "purple") +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.2)) +
  labs(
    title = "Raíces de las transformaciones candidatas - Serie 2",
    x = "Parte real",
    y = "Parte imaginaria"
  ) +
  theme_minimal()

# Varianza muestral y autocorrelacion
# Serie original y sus transformaciones
y2 <- serie2$y

y2_d1 <- diferenciar(y2, d = 1)
y2_d2 <- diferenciar(y2, d = 2)

# Tabla de varianzas y autocorrelaciones
tabla_serie2 <- data.frame(
  Transformacion = c(
    "Original",
    "Primera diferencia",
    "Segunda diferencia"
  ),
  Varianza = c(
    var(y2),
    var(y2_d1),
    var(y2_d2)
  ),
  r1 = c(
    acf_muestral(y2, 1)[1],
    acf_muestral(y2_d1, 1)[1],
    acf_muestral(y2_d2, 1)[1]
  )
)

tabla_serie2$Varianza <- round(tabla_serie2$Varianza, 4)
tabla_serie2$r1 <- round(tabla_serie2$r1, 4)

tabla_serie2

# Medias observadas
media_d1_2 <- mean(y2_d1)
media_d2_2 <- mean(y2_d2)

# Medias esperadas
media_d1_esperada_2 <- (y2[length(y2)] - y2[1]) /
  (length(y2) - 1)

media_d2_esperada_2 <- (
  (y2[length(y2)] - y2[length(y2) - 1]) -
    (y2[2] - y2[1])
) / (length(y2) - 2)

# Comparación de medias
tabla_medias_serie2 <- data.frame(
  Transformacion = c("Primera diferencia", "Segunda diferencia"),
  Media_observada = c(media_d1_2, media_d2_2),
  Media_esperada = c(media_d1_esperada_2, media_d2_esperada_2)
)

tabla_medias_serie2

# Autocovarianzas de la serie original
n2 <- length(y2)
media_y2 <- mean(y2)

gamma0_2 <- mean((y2 - media_y2)^2)

gamma1_2 <- mean(
  (y2[2:n2] - media_y2) *
    (y2[1:(n2 - 1)] - media_y2)
)

# Aplicar la identidad
varianza_identidad_2 <- 2 * gamma0_2 - 2 * gamma1_2

# Varianza muestral de la primera diferencia
varianza_directa_2 <- var(y2_d1)

data.frame(
  Varianza_identidad = varianza_identidad_2,
  Varianza_directa = varianza_directa_2
)

# Candidata 1: diferencia estacional semanal
raices_serie3_1 <- polinomio_rezago(c(0, 0, 0, 0, 0, 0, -1))

raices_serie3_1

# Candidata 2: diferencia ordinaria y estacional
raices_serie3_2 <- polinomio_rezago(c(-1, 0, 0, 0, 0, 0, -1, 1))

raices_serie3_2

# Definir los puntos del círculo unitario 
circulo <- data.frame(
  x = cos(seq(0, 2 * pi, length.out = 200)),
  y = sin(seq(0, 2 * pi, length.out = 200))
)

# Primera candidata: 1 - B^7
g1_s3 <- ggplot(raices_serie3_1, aes(x = Re(raiz), y = Im(raiz))) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_path(data = circulo, aes(x = x, y = y), color = "gray40", linetype = "dotted") +
  geom_point(color = "purple", size = 3) +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.2)) +
  labs(
    title = "Candidata 1: 1 - B⁷",
    x = "Parte real",
    y = "Parte imaginaria"
  ) +
  theme_minimal()

# Segunda candidata: (1 - B)(1 - B^7)
g2_s3 <- ggplot(raices_serie3_2, aes(x = Re(raiz), y = Im(raiz))) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  geom_path(data = circulo, aes(x = x, y = y), color = "gray40", linetype = "dotted") +
  geom_point(color = "purple", size = 3) +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.2, 1.2)) +
  labs(
    title = "Candidata 2: (1 - B)(1 - B⁷)",
    x = "Parte real",
    y = "Parte imaginaria"
  ) +
  theme_minimal()

# Unir ambos gráficos lado a lado
(g1_s3 | g2_s3) +
  plot_annotation(
    title = "Plano complejo de las raíces de las transformaciones - Serie 3",
    theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
  )

# Serie original
y3 <- serie3$y

# Primera candidata: diferencia estacional de periodo 7
y3_D1 <- diferenciar(y3, D = 1, s = 7)

# Segunda candidata: diferencia ordinaria y estacional
y3_d1_D1 <- diferenciar(y3, d = 1, D = 1, s = 7)

# Comparar varianza y autocorrelación de rezago 1
comparacion_serie3 <- data.frame(
  Serie = c("Original", "Diferencia estacional",
            "Diferencia ordinaria y estacional"),
  Varianza = c(var(y3), var(y3_D1), var(y3_d1_D1)),
  r1 = c(acf_muestral(y3, 1)[1],
         acf_muestral(y3_D1, 1)[1],
         acf_muestral(y3_d1_D1, 1)[1])
)

comparacion_serie3

# Medias observadas
media_original <- mean(y3)
media_D1 <- mean(y3_D1)
media_d1_D1 <- mean(y3_d1_D1)

# Media esperada de la diferencia estacional
media_esperada_D1 <- (sum(y3[8:length(y3)]) -
                        sum(y3[1:(length(y3)-7)])) /
  length(y3_D1)

# Media esperada de la diferencia ordinaria y estacional
media_esperada_d1_D1 <- (y3_D1[length(y3_D1)] -
                           y3_D1[1]) /
  length(y3_d1_D1)

# Tabla de verificación
data.frame(
  Transformacion = c("Diferencia estacional",
                     "Diferencia ordinaria y estacional"),
  Media_observada = c(media_D1, media_d1_D1),
  Media_esperada = c(media_esperada_D1, media_esperada_d1_D1)
)

# Serie después de la diferencia estacional
z <- y3_D1
n <- length(z)
media_z <- mean(z)

# Autocovarianzas en los rezagos 0 y 1
gamma0 <- mean((z - media_z)^2)
gamma1 <- mean((z[2:n] - media_z) *
                 (z[1:(n-1)] - media_z))

# Lado derecho de la identidad
identidad <- 2 * gamma0 - 2 * gamma1

# Varianza de la diferencia ordinaria
varianza_diferencia <- var(diferenciar(z, d = 1))

data.frame(
  Gamma0 = gamma0,
  Gamma1 = gamma1,
  Resultado_identidad = identidad,
  Varianza_diferencia = varianza_diferencia
)

# Serie 1 transformada
serie1_d1 <- diferenciar(serie1$y, d = 1)

# Correlograma

# Serie 1: primera diferencia ordinaria
resultado1 <- correlograma(serie1_d1)
resultado1$grafico

# Ljung Box
r1 <- acf_muestral(serie1_d1, m = 24)

resultado_ljung1 <- ljung_box(
  r = r1,
  T = length(serie1_d1),
  m = 24
)

cat(
  "Serie 1 → Q =", round(resultado_ljung1$estadistico, 4),
  "| gl =", resultado_ljung1$grados_libertad,
  "| valor crítico =", round(resultado_ljung1$valor_critico, 4),
  "| p-valor =", round(resultado_ljung1$valor_p, 4), "\n"
)

res_bloques1 <- comparar_bloques(serie1_d1)

tabla_bloques_s1 <- tibble(
  Serie = "Serie 1",
  Bloque = paste("Bloque", res_bloques1$bloque),
  `Media (Ȳ_b)` = round(res_bloques1$media, 3),
  `Varianza (S²_b)` = round(res_bloques1$varianza, 3)
)

kable(
  tabla_bloques_s1, 
  align = "llcc", 
  caption = "Serie 1: Evaluación por Bloques"
)

res_ic1 <- intervalos_media(serie1_d1)

tabla_ic_s1 <- tibble(
  Serie = "Serie 1",
  Método = c("Ingenuo", "Largo plazo (Bartlett)"),
  Media = round(res_ic1$media, 4),
  `Error estándar` = round(res_ic1$error_estandar, 4),
  `IC 95 % inferior` = round(res_ic1$inferior, 4),
  `IC 95 % superior` = round(res_ic1$superior, 4)
)


kable(
  tabla_ic_s1, 
  align = "llccccc",
  caption = "Serie 1: Intervalos de confianza al 95 % para la media"
)

# Series transformada
serie2_d1 <- diferenciar(serie2$y, d = 1)

# Correlograma de la serie transformada

# Serie 2: primera diferencia ordinaria
resultado2 <- correlograma(serie2_d1)
resultado2$grafico

#Ljung Box
r2 <- acf_muestral(serie2_d1, m = 24)
resultado_ljung2 <- ljung_box(r2, T = length(serie2_d1), m = 24)

cat(
  "Serie 2 → Q =", round(resultado_ljung2$estadistico, 4),
  "| gl =", resultado_ljung2$grados_libertad,
  "| valor crítico =", round(resultado_ljung2$valor_critico, 4),
  "| p-valor =", round(resultado_ljung2$valor_p, 4), "\n"
)

res_bloques2 <- comparar_bloques(serie2_d1)

# Reorganizamos
tabla_bloques_s2 <- tibble(
  Serie = "Serie 2",
  Bloque = paste("Bloque", res_bloques2$bloque),
  `Media (Ȳ_b)` = round(res_bloques2$media, 3),
  `Varianza (S²_b)` = round(res_bloques2$varianza, 3)
)


kable(
  tabla_bloques_s2, 
  align = "llcc", 
  caption = "Serie 2: Evaluación por Bloques"
)

res2 <- intervalos_media(serie2_d1)

tabla_ic_s2 <- tibble(
  Serie = "Serie 2",
  Método = c("Ingenuo", "Largo plazo (Bartlett)"),
  Media = round(res2$media, 4),
  `Error estándar` = round(res2$error_estandar, 4),
  `IC 95 % inferior` = round(res2$inferior, 4),
  `IC 95 % superior` = round(res2$superior, 4)
)


kable(tabla_ic_s2, align = "llccccc",
      caption = "Serie 2: Intervalos de confianza al 95 % para la media")

# Series transformada
serie3_D1 <- diferenciar(serie3$y, D = 1, s = 7)

# Correlograma de la serie transformada

# Serie 3: diferencia estacional de período 7
resultado3 <- correlograma(serie3_D1)
resultado3$grafico

#Ljung Box
r3 <- acf_muestral(serie3_D1, m = 14)
resultado_ljung3 <- ljung_box(r3, T = length(serie3_D1), m = 14)
cat(
  "Serie 3 → Q =", round(resultado_ljung3$estadistico, 4),
  "| gl =", resultado_ljung3$grados_libertad,
  "| valor crítico =", round(resultado_ljung3$valor_critico, 4),
  "| p-valor =", round(resultado_ljung3$valor_p, 4), "\n"
)

res_bloques3 <- comparar_bloques(serie3_D1)


tabla_bloques_s3 <- tibble(
  Serie = "Serie 3",
  Bloque = paste("Bloque", res_bloques3$bloque),
  `Media (Ȳ_b)` = round(res_bloques3$media, 3),
  `Varianza (S²_b)` = round(res_bloques3$varianza, 3)
)

kable(
  tabla_bloques_s3, 
  align = "llcc", 
  caption = "Serie 3: Evaluación por Bloques"
)

res_ic3 <- intervalos_media(serie3_D1)


tabla_ic_s3 <- tibble(
  Serie = "Serie 3",
  Método = c("Ingenuo", "Largo plazo (Bartlett)"),
  Media = round(res_ic3$media, 4),
  `Error estándar` = round(res_ic3$error_estandar, 4),
  `IC 95 % inferior` = round(res_ic3$inferior, 4),
  `IC 95 % superior` = round(res_ic3$superior, 4)
)


kable(
  tabla_ic_s3, 
  align = "llccccc",
  caption = "Serie 3: Intervalos de confianza al 95 % para la media"
)

tabla_criterios <- tibble(
  `Método del Catálogo` = c(
    "Media Simple", "Ingenuo", "Ingenuo Estacional (s=7)", "Media Móvil (k)",
    "SES (α)", "Doble Media Móvil (k)", "Tendencia Lineal (OLS)",
    "Tendencia Cuadrática (OLS)", "Tendencia Exponencial (OLS)",
    "Tendencia con Ficticias (Suma 0)", "Holt (α, β)",
    "Holt-Winters Aditivo", "Holt-Winters Multiplicativo"
  ),
  `Serie 1 (S1)` = c(
    "Contraste", "Aplicado", "No aplicado", "No aplicado",
    "No aplicado", "Aplicado", "No aplicado",
    "Aplicado", "No aplicado",
    "No aplicado", "Aplicado",
    "No aplicado", "No aplicado"
  ),
  `Serie 2 (S2)` = c(
    "No aplicado", "Aplicado", "No aplicado", "Contraste",
    "No aplicado", "Aplicado", "Aplicado",
    "No aplicado", "Aplicado",
    "No aplicado", "Aplicado",
    "No aplicado", "No aplicado"
  ),
  `Serie 3 (S3)` = c(
    "Aplicado", "No aplicado", "Aplicado", "Aplicado",
    "Aplicado", "No aplicado", "Contraste",
    "No aplicado", "No aplicado",
    "Aplicado", "No aplicado",
    "Aplicado", "Aplicado"
  ),
  `Criterio de Selección` = c(
    "Asume media constante global. En S1 y S2 falla por ignorar la tendencia; en S3 es adecuada por estacionariedad en nivel.",
    "Predice con el último valor observado. Adecuado para caminata aleatoria en S1 y cambios locales en S2. Inadecuado en S3 por omitir la estacionalidad",
    "Repite el patrón del período s=7. Indispensable en S3 por su estacionalidad; descartado en S1 y S2 por ausencia de estacionalidad",
    "Promedia ventanas cortas para filtrar ruido. Válido en S3 para suavizar en nivel; no aplicado en S1 y S2 usado para el contraste",
    "Suaviza nivel con memoria adaptativa. Adecuado en S3 al ser estacionaria en media; no aplicado en S1 y S2 por requerir ajuste de pendiente.",
    "Elimina el sesgo de la Media Móvil ante tendencias locales. Ideal para S1 y S2; innecesario en S3 por ausencia de tendencia",
    "Modela pendiente determinística fija en t. Adecuado para el crecimiento sostenido de S2; no aplicado en S1 por no tener una pendiente fija y contraste en S3 al forzar pendiente a una serie estacionaria",
    "Modela curvatura o aceleración t^2. Usado en la serie 1 por sospecha en la inspección visual, descartado en las demás",
    "Modela crecimiento porcentual sobre log(Yt). Evaluado en S2 por la dirección creciente; omitido en S1 y S3 por falta de crecimiento exponencial.",
    "Combina el nivel con s-1 ficticias estacionales de suma cero. Diseñado específicamente para capturar la estructura de días en S3.",
    "Adapta dinámicamente nivel local y pendiente cambiante. Diseñado para series no estacionales con tendencia (S1 y S2); no aplicado en S3.",
    "Captura nivel, pendiente y estacionalidad de amplitud constante (s=7). Evaluado en S3 para modelar su patrón.",
    "Captura nivel, pendiente y estacionalidad proporcional al nivel (s=7). Evaluado en S3 para modelar su patrón."
  )
)


kable(
  tabla_criterios,
  caption = "Matriz Criterio de Selección y Asignación de Métodos por Serie",
  align = c("l", "c", "c", "c", "l")
)

# Tramo de estimación
y1_est <- serie1$y[1:n0_1]

# Metodo ingenuo
modelo_ingenuo_1 <- ajustar_ingenuo(y1_est)

h <- particion$H[1]

# Pronosticos del ingenuo
pronosticos_ingenuo <- modelo_ingenuo_1$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos ingenuos:", round(pronosticos_ingenuo, 4), "\n"
)

# K optimo
rejilla_k <- 3:12

opt_dmm_1 <- optimizar(
  y = y1_est,
  metodo = "dmm",
  rejilla = rejilla_k
)

opt_dmm_1$rejilla

# Graficar k optimo
graficar_optimizacion(
  opt_dmm_1,
  parametro = "k",
  titulo = "Optimización de la doble media móvil - Serie 1"
)

# Ajustar DMM con k optimo
k_opt_1 <- opt_dmm_1$optimo$k

modelo_dmm_1 <- ajustar_dmm(y1_est, k_opt_1)

# Pronosticos
pronosticos_dmm <- modelo_dmm_1$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos DMM:", round(pronosticos_dmm, 4), "\n"
)

# Ajustar Tendencia Cuadrática
modelo_tendencia_1 <- ajustar_tendencia(
  y = y1_est,
  tipo = "cuadratica",
  corregir_sesgo = FALSE
)

# Pronosticos
pronosticos_tendencia <- modelo_tendencia_1$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos de tendencia cuadrática:",
  round(pronosticos_tendencia, 4), "\n"
)

tabla_coef_1 <- modelo_tendencia_1$parametros$coeficientes

tabla_coef_1$estimacion   <- round(tabla_coef_1$estimacion, 4)
tabla_coef_1$se_ordinario <- round(tabla_coef_1$se_ordinario, 4)
tabla_coef_1$se_robusto   <- round(tabla_coef_1$se_robusto, 4)
tabla_coef_1$t            <- round(tabla_coef_1$t, 4)
tabla_coef_1$valor_p      <- format.pval(tabla_coef_1$valor_p, digits = 4, eps = 0.0001)

tabla_coef_1

residuos_1 <- modelo_tendencia_1$parametros$residuos

diagnostico_tendencia_1 <- validar_errores(
  e = residuos_1,
  m = NULL,
  p = 3
)

diagnostico_tendencia_1$correlograma$grafico

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_1$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_1$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_1$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    diagnostico_tendencia_1$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_1$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_1$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_1$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    diagnostico_tendencia_1$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

diagnostico_tendencia_1$durbin_watson <-
  durbin_watson(modelo_tendencia_1$parametros$residuos)

cat(
  "Estadístico de prueba:",
  round(diagnostico_tendencia_1$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(diagnostico_tendencia_1$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(diagnostico_tendencia_1$durbin_watson$dL, 4), "\n",
  "dU:",
  round(diagnostico_tendencia_1$durbin_watson$dU, 4), "\n"
)

# Alfa y Beta optimos
rejilla_holt <- seq(0.05, 0.95, by = 0.05)

opt_holt_1 <- optimizar(
  y = y1_est,
  metodo = "holt",
  rejilla = rejilla_holt
)

opt_holt_1$rejilla

# Graficar alfa, beta optimos
graficar_holt(opt_holt_1)

# Ajustar el modelo
alpha_opt_1 <- opt_holt_1$optimo$alpha
beta_opt_1 <- opt_holt_1$optimo$beta

modelo_holt_1 <- ajustar_holt(
  y1_est,
  alpha_opt_1,
  beta_opt_1
)

# Pronosticos
pronosticos_holt <- modelo_holt_1$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos Holt:", round(pronosticos_holt, 4), "\n"
)

# Media simple
modelo_media <- ajustar_media(y1_est)

pronosticos_media_simple <- modelo_media$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos de media simple:",
  round(pronosticos_media_simple, 4), "\n"
)

# Evaluacion de los metodos
eval_s1_ing       <- origen_movil(serie1$y, fn_ajuste = ajustar_ingenuo, H = 4, n_eval = 40, s = 1)

eval_s1_dmm       <- origen_movil(serie1$y, fn_ajuste = ajustar_dmm, H = 4, n_eval = 40, s = 1, k = opt_dmm_1$optimo$k)

eval_s1_holt      <- origen_movil(serie1$y, fn_ajuste = ajustar_holt, H = 4, n_eval = 40, s = 1, alpha =
                                    opt_holt_1$optimo$alpha, beta = opt_holt_1$optimo$beta)

eval_s1_cuad      <- origen_movil(serie1$y, fn_ajuste = ajustar_tendencia, H = 4, n_eval = 40, s = 1, tipo =
                                    "cuadratica")

eval_s1_media_con <- origen_movil(serie1$y, fn_ajuste = ajustar_media, H = 4, n_eval = 40, s = 1)

tabla_s1_h1 <- tibble(
  `Método de Pronóstico` = c(
    "Referente Ingenuo (Simple)", 
    "Doble Media Móvil (DMM)", 
    "Holt Lineal", 
    "Tendencia Cuadrática MCO", 
    "Contraste: Media Simple"
  ),
  RMSE = c(
    eval_s1_ing$h1$RMSE, 
    eval_s1_dmm$h1$RMSE, 
    eval_s1_holt$h1$RMSE, 
    eval_s1_cuad$h1$RMSE, 
    eval_s1_media_con$h1$RMSE
  ),
  MAD = c(
    eval_s1_ing$h1$MAD, 
    eval_s1_dmm$h1$MAD, 
    eval_s1_holt$h1$MAD, 
    eval_s1_cuad$h1$MAD, 
    eval_s1_media_con$h1$MAD
  ),
  `MAPE (%)` = c(
    eval_s1_ing$h1$MAPE, 
    eval_s1_dmm$h1$MAPE, 
    eval_s1_holt$h1$MAPE, 
    eval_s1_cuad$h1$MAPE, 
    eval_s1_media_con$h1$MAPE
  )
) %>% 
  arrange(RMSE) %>% 
  mutate(
    RMSE = round(RMSE, 3),
    MAD  = round(MAD, 3),
    `MAPE (%)` = round(`MAPE (%)`, 3)
  )


kable(
  tabla_s1_h1,
  align = "lccc",
  caption = "Serie 1: Medidas de error para horizonte h = 1 (Origen Móvil)",
  booktabs = TRUE
)

# h = 4
tabla_s1_h4 <- tibble(
  `Método de Pronóstico` = c(
    "Referente Ingenuo (Simple)", 
    "Doble Media Móvil (DMM)", 
    "Holt Lineal", 
    "Tendencia Cuadrática MCO", 
    "Contraste: Media Simple"
  ),
  RMSE = c(
    eval_s1_ing$hH$RMSE, 
    eval_s1_dmm$hH$RMSE, 
    eval_s1_holt$hH$RMSE, 
    eval_s1_cuad$hH$RMSE, 
    eval_s1_media_con$hH$RMSE
  ),
  MAD = c(
    eval_s1_ing$hH$MAD, 
    eval_s1_dmm$hH$MAD, 
    eval_s1_holt$hH$MAD, 
    eval_s1_cuad$hH$MAD, 
    eval_s1_media_con$hH$MAD
  ),
  `MAPE (%)` = c(
    eval_s1_ing$hH$MAPE, 
    eval_s1_dmm$hH$MAPE, 
    eval_s1_holt$hH$MAPE, 
    eval_s1_cuad$hH$MAPE, 
    eval_s1_media_con$hH$MAPE
  )
) %>% 
  arrange(RMSE) %>% 
  mutate(
    RMSE = round(RMSE, 3),
    MAD  = round(MAD, 3),
    `MAPE (%)` = round(`MAPE (%)`, 3)
  )


kable(
  tabla_s1_h4,
  align = "lccc",
  caption = "Serie 1: Medidas de error para horizonte h = 4 (Origen Móvil)",
  booktabs = TRUE
)

# Errores de Holt en origen móvil a un paso
errores_holt_1 <- eval_s1_holt$e_h1

# Validar los errores
resultado_errores_holt_1 <- validar_errores(
  e = errores_holt_1,
  m = NULL,
  p = 2
)

g1_s1 <- resultado_errores_holt_1$grafico_errores

g2_s1 <- resultado_errores_holt_1$correlograma$grafico

g1_s1 | g2_s1

cat(
  "Estadístico de prueba:", round(resultado_errores_holt_1$t, 4), "\n",
  "Grados de libertad:", resultado_errores_holt_1$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_holt_1$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_holt_1$valor_p_t, 4), "\n"
)

cat(
  "Estadístico de prueba:", round(
    resultado_errores_holt_1$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_holt_1$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_holt_1$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_holt_1$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    resultado_errores_holt_1$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_holt_1$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_holt_1$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_holt_1$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

cat(
  "Estadístico de prueba:",
  round(resultado_errores_holt_1$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(resultado_errores_holt_1$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(resultado_errores_holt_1$durbin_watson$dL, 4), "\n",
  "dU:",
  round(resultado_errores_holt_1$durbin_watson$dU, 4), "\n"
)

T_1 <- length(serie1$y)
H_1 <- 4

# Ajustar Holt usando toda la serie 1
modelo_final_1 <- ajustar_holt(
  serie1$y,
  alpha = 0.30,
  beta = 0.05
)

# Pronósticos de las H semanas siguientes
pronosticos_finales_1 <- modelo_final_1$pronosticar(H_1)

# Desviación estándar de los errores para cada horizonte
desv_errores_1 <- sapply(1:4, function(h) {
  errores <- eval_s1_holt$todos$error[
    eval_s1_holt$todos$h == h
  ]
  sd(errores)
})

# Datos para el gráfico
grafico_final_1 <- data.frame(
  tiempo = 1:(T_1 + H_1),
  real = c(serie1$y, rep(NA, H_1)),
  ajuste = c(
    modelo_final_1$yhat,
    rep(NA, H_1)
  ),
  pronostico = c(
    rep(NA, T_1),
    pronosticos_finales_1
  ),
  inferior = c(
    rep(NA, T_1),
    pronosticos_finales_1 - 1.96 * desv_errores_1
  ),
  superior = c(
    rep(NA, T_1),
    pronosticos_finales_1 + 1.96 * desv_errores_1
  )
)

# Gráfico final
ggplot2::ggplot(
  grafico_final_1,
  ggplot2::aes(x = tiempo)
) +
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = inferior),
    color = "black",
    linetype = "dotted"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = superior),
    color = "black",
    linetype = "dotted"
  ) +
  ggplot2::geom_vline(
    xintercept = T_1,
    color = "red",
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Serie 1: ajuste y pronóstico final de Holt",
    subtitle = "Límites heurísticos de ±1,96 desviaciones estándar",
    x = "Semana",
    y = "Valor promedio de compra (miles de pesos)"
  ) +
  ggplot2::theme_minimal()

# Para los ultimos periodos
ganador_s1 <- "Holt Lineal"
fn_s1 <- function(y) ajustar_holt(y, alpha = opt_holt_1$optimo$alpha, beta = opt_holt_1$optimo$beta)

final_s1 <- pronostico_final(
  y = serie1$y, fechas = serie1$fecha, fn = fn_s1, H = 4, n_eval = 40, paso = 7,
  titulo = paste0("Serie 1: pronóstico final a 4 semanas (", ganador_s1, ")"),
  unidad = attr(serie1, "unidad")
)

final_s1$grafico

tabla_final(final_s1, "Serie 1: Pronóstico final y banda heurística")

# Comparación con la meta de 510 mil COP
meta_s1 <- 510
col_sup <- grep("sup", names(final_s1$tabla), ignore.case = TRUE, value = TRUE)[1]
n_punt  <- sum(final_s1$tabla$pronostico < meta_s1)
texto_s1 <- paste0(
  "el pronóstico puntual queda por debajo de la meta en ", n_punt, " de las 4 semanas"
)
if (!is.na(col_sup)) {
  n_sup <- sum(final_s1$tabla[[col_sup]] < meta_s1)
  texto_s1 <- paste0(texto_s1, " y el límite superior de la banda en ", n_sup, " de las 4 semanas")
}
texto_s1 <- paste0(texto_s1, ".")

# Tramo de estimación
y2_est <- serie2$y[1:n0_2]

# Metodo ingenuo
modelo_ingenuo_2 <- ajustar_ingenuo(y2_est)

h <- particion$H[2]

# Pronosticos del ingenuo
pronosticos_ingenuo2 <- modelo_ingenuo_2$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos ingenuos:", round(pronosticos_ingenuo2, 4), "\n"
)

# K optimo
rejilla_k <- 3:12

opt_dmm_2 <- optimizar(
  y = y2_est,
  metodo = "dmm",
  rejilla = rejilla_k
)

opt_dmm_2$rejilla

# Graficar k optimo
graficar_optimizacion(
  opt_dmm_2,
  parametro = "k",
  titulo = "Optimización de la doble media móvil - Serie 2"
)

# Ajustar DMM con k optimo
k_opt_2 <- opt_dmm_2$optimo$k

modelo_dmm_2 <- ajustar_dmm(y2_est, k_opt_2)

# Pronosticos
pronosticos_dmm2 <- modelo_dmm_2$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos DMM:", round(pronosticos_dmm2, 4), "\n"
)

# Ajustar Tendencia lineal
modelo_tendencia_2 <- ajustar_tendencia(
  y = y2_est,
  tipo = "lineal",
  corregir_sesgo = FALSE
)

# Pronosticos
pronosticos_tendencia2 <- modelo_tendencia_2$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos de tendencia lineal:",
  round(pronosticos_tendencia2, 4), "\n"
)

tabla_coef_2 <- modelo_tendencia_2$parametros$coeficientes

tabla_coef_2$estimacion   <- round(tabla_coef_2$estimacion, 4)
tabla_coef_2$se_ordinario <- round(tabla_coef_2$se_ordinario, 4)
tabla_coef_2$se_robusto   <- round(tabla_coef_2$se_robusto, 4)
tabla_coef_2$t            <- round(tabla_coef_2$t, 4)
tabla_coef_2$valor_p      <- format.pval(tabla_coef_2$valor_p, digits = 4, eps = 0.0001)

tabla_coef_2

residuos_2 <- modelo_tendencia_2$parametros$residuos

diagnostico_tendencia_2 <- validar_errores(
  e = residuos_2,
  m = NULL,
  p = 2
)

diagnostico_tendencia_2$correlograma$grafico

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_2$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_2$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_2$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    diagnostico_tendencia_2$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_2$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_2$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_2$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    diagnostico_tendencia_2$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

diagnostico_tendencia_2$durbin_watson <-
  durbin_watson(modelo_tendencia_2$parametros$residuos)

cat(
  "Estadístico de prueba:",
  round(diagnostico_tendencia_2$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(diagnostico_tendencia_2$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(diagnostico_tendencia_2$durbin_watson$dL, 4), "\n",
  "dU:",
  round(diagnostico_tendencia_2$durbin_watson$dU, 4), "\n"
)

# Ajustar Tendencia Cuadrática
modelo_tendencia_exp <- ajustar_tendencia(
  y = y2_est,
  tipo = "exponencial",
  corregir_sesgo = TRUE
)

# Pronosticos
pronosticos_tendencia_exp <- modelo_tendencia_exp$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos de tendencia exponencial:",
  round(pronosticos_tendencia_exp, 4), "\n"
)

tabla_coef_exp <- modelo_tendencia_exp$parametros$coeficientes

tabla_coef_exp$estimacion   <- round(tabla_coef_exp$estimacion, 4)
tabla_coef_exp$se_ordinario <- round(tabla_coef_exp$se_ordinario, 4)
tabla_coef_exp$se_robusto   <- round(tabla_coef_exp$se_robusto, 4)
tabla_coef_exp$t            <- round(tabla_coef_exp$t, 4)
tabla_coef_exp$valor_p      <- format.pval(tabla_coef_exp$valor_p, digits = 4, eps = 0.0001)

tabla_coef_exp

residuos_exp <- modelo_tendencia_exp$parametros$residuos

diagnostico_tendencia_exp <- validar_errores(
  e = residuos_exp,
  m = NULL,
  p = 2
)

diagnostico_tendencia_exp$correlograma$grafico

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_exp$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_exp$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_exp$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    diagnostico_tendencia_exp$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_exp$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_exp$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_exp$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    diagnostico_tendencia_exp$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

diagnostico_tendencia_exp$durbin_watson <-
  durbin_watson(modelo_tendencia_exp$parametros$residuos)

cat(
  "Estadístico de prueba:",
  round(diagnostico_tendencia_exp$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(diagnostico_tendencia_exp$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(diagnostico_tendencia_exp$durbin_watson$dL, 4), "\n",
  "dU:",
  round(diagnostico_tendencia_exp$durbin_watson$dU, 4), "\n"
)

# Alfa y Beta optimos
rejilla_holt <- seq(0.05, 0.95, by = 0.05)

opt_holt_2 <- optimizar(
  y = y2_est,
  metodo = "holt",
  rejilla = rejilla_holt
)

opt_holt_2$rejilla

# Graficar alfa, beta optimos
graficar_holt(opt_holt_2)

# Ajustar el modelo
alpha_opt_2 <- opt_holt_2$optimo$alpha
beta_opt_2 <- opt_holt_2$optimo$beta

modelo_holt_2 <- ajustar_holt(
  y2_est,
  alpha_opt_2,
  beta_opt_2
)

# Pronosticos
pronosticos_holt2 <- modelo_holt_2$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronósticos Holt:", round(pronosticos_holt2, 4), "\n"
)

# Optimización de la ventana de la media móvil
resultado_opt_mm <- optimizar(
  y = y2_est,
  metodo = "mm",
  rejilla = 2:12
)

resultado_opt_mm$rejilla

#Gráfica de optimización
grafico_opt_mm <- graficar_optimizacion(
  resultado = resultado_opt_mm,
  parametro = "k",
  titulo = "Optimización de la ventana de media móvil - Serie 2"
)
grafico_opt_mm

# Ajuste de la media móvil con el k óptimo
modelo_mm2 <- ajustar_mm(
  y = y2_est,
  k = resultado_opt_mm$optimo$k
)

# Pronóstico de las semanas de validación
pronosticos_mm2 <- modelo_mm2$pronosticar(h)

cat(
  "Horizonte de validación (h):", h, "\n",
  "Pronóstico constante:", round(pronosticos_mm2[1], 4), "\n"
)

eval_s2_ing       <- origen_movil(serie2$y, fn_ajuste = ajustar_ingenuo, H = 6, n_eval = 36, s = 1)

eval_s2_dmm       <- origen_movil(serie2$y, fn_ajuste = ajustar_dmm, H = 6, n_eval = 36, s = 1, k = opt_dmm_2$optimo$k)

eval_s2_lin       <- origen_movil(serie2$y, fn_ajuste = ajustar_tendencia, H = 6, n_eval = 36, s = 1, tipo = "lineal")

eval_s2_exp       <- origen_movil(serie2$y, fn_ajuste = ajustar_tendencia, H = 6, n_eval = 36, s = 1, tipo =
                                    "exponencial")

eval_s2_holt      <- origen_movil(serie2$y, fn_ajuste = ajustar_holt, H = 6, n_eval = 36, s = 1, alpha =
                                    opt_holt_2$optimo$alpha, beta = opt_holt_2$optimo$beta)

eval_s2_mm_con <- origen_movil(serie2$y, fn_ajuste = ajustar_mm, H = 6, n_eval = 36, s = 1, k = resultado_opt_mm$optimo$k)

# Errores para h = 1

tabla_s2_h1 <- tibble(
  `Método de Pronóstico` = c(
    "Referente Ingenuo (Simple)", 
    "Doble Media Móvil (DMM)", 
    "Tendencia Lineal MCO", 
    "Tendencia Exponencial MCO", 
    "Holt Lineal", 
    "Contraste: Media Movil (MM)"
  ),
  RMSE = c(
    eval_s2_ing$h1$RMSE, 
    eval_s2_dmm$h1$RMSE, 
    eval_s2_lin$h1$RMSE, 
    eval_s2_exp$h1$RMSE, 
    eval_s2_holt$h1$RMSE, 
    eval_s2_mm_con$h1$RMSE
  ),
  MAD = c(
    eval_s2_ing$h1$MAD, 
    eval_s2_dmm$h1$MAD, 
    eval_s2_lin$h1$MAD, 
    eval_s2_exp$h1$MAD, 
    eval_s2_holt$h1$MAD, 
    eval_s2_mm_con$h1$MAD
  ),
  `MAPE (%)` = c(
    eval_s2_ing$h1$MAPE, 
    eval_s2_dmm$h1$MAPE, 
    eval_s2_lin$h1$MAPE, 
    eval_s2_exp$h1$MAPE, 
    eval_s2_holt$h1$MAPE, 
    eval_s2_mm_con$h1$MAPE
  )
) %>% 
  arrange(RMSE) %>% 
  mutate(
    RMSE = round(RMSE, 3),
    MAD  = round(MAD, 3),
    `MAPE (%)` = round(`MAPE (%)`, 3)
  )


kable(
  tabla_s2_h1,
  align = "lccc",
  caption = "Serie 2: Medidas de error para horizonte h = 1 (Origen Móvil)",
  booktabs = TRUE
)

# Errores para h = 6
tabla_s2_h6 <- tibble(
  `Método de Pronóstico` = c(
    "Referente Ingenuo (Simple)", 
    "Doble Media Móvil (DMM)", 
    "Tendencia Lineal MCO", 
    "Tendencia Exponencial MCO", 
    "Holt Lineal", 
    "Contraste: Media Movil (MM)"
  ),
  RMSE = c(
    eval_s2_ing$hH$RMSE, 
    eval_s2_dmm$hH$RMSE, 
    eval_s2_lin$hH$RMSE, 
    eval_s2_exp$hH$RMSE, 
    eval_s2_holt$hH$RMSE, 
    eval_s2_mm_con$hH$RMSE
  ),
  MAD = c(
    eval_s2_ing$hH$MAD, 
    eval_s2_dmm$hH$MAD, 
    eval_s2_lin$hH$MAD, 
    eval_s2_exp$hH$MAD, 
    eval_s2_holt$hH$MAD, 
    eval_s2_mm_con$hH$MAD
  ),
  `MAPE (%)` = c(
    eval_s2_ing$hH$MAPE, 
    eval_s2_dmm$hH$MAPE, 
    eval_s2_lin$hH$MAPE, 
    eval_s2_exp$hH$MAPE, 
    eval_s2_holt$hH$MAPE, 
    eval_s2_mm_con$hH$MAPE
  )
) %>% 
  arrange(RMSE) %>% 
  mutate(
    RMSE = round(RMSE, 3),
    MAD  = round(MAD, 3),
    `MAPE (%)` = round(`MAPE (%)`, 3)
  )


kable(
  tabla_s2_h6,
  align = "lccc",
  caption = "Serie 2: Medidas de error para horizonte h = 6 (Origen Móvil)",
  booktabs = TRUE
)

# Errores de Holt en origen móvil a un paso
errores_holt_2 <- eval_s2_holt$e_h1

# Validar los errores
resultado_errores_holt_2 <- validar_errores(
  e = errores_holt_2,
  m = NULL,
  p = 2
)

g1_s2 <- resultado_errores_holt_2$grafico_errores
g2_s2 <- resultado_errores_holt_2$correlograma$grafico

g1_s2 | g2_s2

cat(
  "Estadístico de prueba:", round(resultado_errores_holt_2$t, 4), "\n",
  "Grados de libertad:", resultado_errores_holt_2$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_holt_2$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_holt_2$valor_p_t, 4), "\n"
)

cat(
  "Estadístico de prueba:", round(
    resultado_errores_holt_2$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_holt_2$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_holt_2$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_holt_2$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    resultado_errores_holt_2$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_holt_2$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_holt_2$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_holt_2$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

cat(
  "Estadístico de prueba:",
  round(resultado_errores_holt_2$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(resultado_errores_holt_2$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(resultado_errores_holt_2$durbin_watson$dL, 4), "\n",
  "dU:",
  round(resultado_errores_holt_2$durbin_watson$dU, 4), "\n"
)

T_2 <- length(serie2$y)
H_2 <- 6

# Ajustar Holt usando toda la serie 2
modelo_final_2 <- ajustar_holt(
  serie2$y,
  alpha = opt_holt_2$optimo$alpha,
  beta = opt_holt_2$optimo$beta
)

# Pronósticos de las H semanas siguientes
pronosticos_finales_2 <- modelo_final_2$pronosticar(H_2)

# Desviación estándar de los errores para cada horizonte
desv_errores_2 <- sapply(1:6, function(h) {
  errores <- eval_s2_holt$todos$error[
    eval_s2_holt$todos$h == h
  ]
  sd(errores)
})

# Datos para el gráfico
grafico_final_2 <- data.frame(
  tiempo = 1:(T_2 + H_2),
  real = c(serie2$y, rep(NA, H_2)),
  ajuste = c(
    modelo_final_2$yhat,
    rep(NA, H_2)
  ),
  pronostico = c(
    rep(NA, T_2),
    pronosticos_finales_2
  ),
  inferior = c(
    rep(NA, T_2),
    pronosticos_finales_2 - 1.96 * desv_errores_2
  ),
  superior = c(
    rep(NA, T_2),
    pronosticos_finales_2 + 1.96 * desv_errores_2
  )
)

# Gráfico final
ggplot2::ggplot(
  grafico_final_2,
  ggplot2::aes(x = tiempo)
) +
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = inferior),
    color = "black",
    linetype = "dotted"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = superior),
    color = "black",
    linetype = "dotted"
  ) +
  ggplot2::geom_vline(
    xintercept = T_2,
    color = "red",
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Serie 2: ajuste y pronóstico final de Holt",
    subtitle = "Límites heurísticos de ±1,96 desviaciones estándar",
    x = "Semana",
    y = "Valor cientos de clientes"
  ) +
  ggplot2::theme_minimal()

# Pronostico de los ultimos 
ganador_s2 <- "Holt Lineal"
fn_s2 <- function(y) ajustar_holt(y, alpha = opt_holt_2$optimo$alpha, beta = opt_holt_2$optimo$beta)

final_s2 <- pronostico_final(
  y = serie2$y, fechas = serie2$fecha, fn = fn_s2, H = 6, n_eval = 36, paso = 7,
  titulo = paste0("Serie 2: pronóstico final a 6 semanas (", ganador_s2, ")"),
  unidad = attr(serie2, "unidad")
)

final_s2$grafico

tabla_final(final_s2, "Serie 2: Pronóstico final y banda heurística")

# Tramo de estimación
y3_est <- serie3$y[1:n0_3]
h_3 <- particion$H[3]

#Ajustar Media Simple
modelo_ms <- ajustar_media(
  y3_est
)

#Pronósticos
pronosticos_ms <- modelo_ms$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronóstico constante:", round(pronosticos_ms[1], 7), "\n"
)

modelo_ingenuo_3 <- ajustar_ingenuo_estacional(y3_est, 7)

# Pronosticos del ingenuo estacional
pronosticos_ingenuo3 <- modelo_ingenuo_3$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronósticos ingenuo estacional:", round(pronosticos_ingenuo3, 7), "\n"
)

# Optimización de la ventana de la media móvil
resultado_opt_3 <- optimizar(
  y = y3_est,
  metodo = "mm",
  rejilla = 2:12
)

resultado_opt_3$rejilla

#Gráfica de optimización
grafico_opt_3 <- graficar_optimizacion(
  resultado = resultado_opt_3,
  parametro = "k",
  titulo = "Optimización de la ventana de media móvil - Serie 3"
)

grafico_opt_3

# Ajuste de la media móvil con el k óptimo
modelo_mm3 <- ajustar_mm(
  y = y3_est,
  k = resultado_opt_3$optimo$k
)


# Pronóstico 
pronosticos_mm3 <- modelo_mm3$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronóstico constante:", round(pronosticos_mm3[1], 7), "\n"
)

resultado_opt_ses3 <- optimizar(
  y = y3_est,
  metodo = "ses",
  rejilla = seq(0.02, 0.98, by = 0.1)
)

resultado_opt_ses3$rejilla

grafico_opt_ses3 <- graficar_optimizacion(
  resultado = resultado_opt_ses3,
  parametro = "alpha",
  titulo = "Optimización de alpha - Serie 3"
)

grafico_opt_ses3

modelo_ses3 <- ajustar_ses(
  y = y3_est,
  alpha = resultado_opt_ses3$optimo$alpha
)

pronosticos_ses3 <- modelo_ses3$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Alpha óptimo:", resultado_opt_ses3$optimo$alpha, "\n",
  "Pronóstico constante:", round(pronosticos_ses3[1], 7), "\n"
)

modelo_tf3 <- ajustar_tendencia_estacional(y3_est, 7)

pronosticos_tf3 <- modelo_tf3$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronóstico Tendencia ficticia:", round(pronosticos_tf3[1:7], 7), "\n"
)

tabla_coef_tf3 <- modelo_tf3$parametros$coeficientes

tabla_coef_tf3$estimacion   <- round(tabla_coef_tf3$estimacion, 4)
tabla_coef_tf3$se_ordinario <- round(tabla_coef_tf3$se_ordinario, 4)
tabla_coef_tf3$se_robusto   <- round(tabla_coef_tf3$se_robusto, 4)
tabla_coef_tf3$t            <- round(tabla_coef_tf3$t, 4)
tabla_coef_tf3$valor_p      <- format.pval(tabla_coef_tf3$valor_p, digits = 4, eps = 0.0001)

tabla_coef_tf3

residuos_tf3 <- modelo_tf3$parametros$residuos

diagnostico_tf3 <- validar_errores(
  e = residuos_tf3,
  m = NULL,
  p = 0
)

diagnostico_tf3$correlograma$grafico

cat(
  "Estadístico de prueba:", round(
    diagnostico_tf3$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tf3$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tf3$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    diagnostico_tf3$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    diagnostico_tf3$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tf3$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tf3$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    diagnostico_tf3$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

diagnostico_tf3$durbin_watson <-
  durbin_watson(modelo_tf3$parametros$residuos)

cat(
  "Estadístico de prueba:",
  round(diagnostico_tf3$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(diagnostico_tf3$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(diagnostico_tf3$durbin_watson$dL, 4), "\n",
  "dU:",
  round(diagnostico_tf3$durbin_watson$dU, 4), "\n"
)

# Alfa, Beta, Gamma optimos
rejilla_hw <- seq(0.1, 0.9, by = 0.1)

opt_holt_ad <- optimizar(
  y = y3_est,
  metodo = "hw",
  rejilla = rejilla_hw,
  s=7
)

opt_holt_ad$rejilla

# Graficar alfa, beta optimos
graficar_hw(opt_holt_ad, "Optimización Alfa, Beta, Gamma - Serie 3")

# Ajustar el modelo
alpha_opt_ad <- opt_holt_ad$optimo$alpha
beta_opt_ad <- opt_holt_ad$optimo$beta
gamma_opt_ad <- opt_holt_ad$optimo$gamma

modelo_holt_ad <- ajustar_hw(
  y3_est,
  s=7,
  alpha_opt_ad,
  beta_opt_ad,
  gamma_opt_ad
)

# Pronosticos
pronosticos_hw_ad <- modelo_holt_ad$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronósticos Holt Winters Aditivo:", round(pronosticos_hw_ad, 7), "\n"
)

# Ajustar modelo
modelo_holt_mult <- ajustar_hw_mult(
  y3_est,
  s=7,
  alpha_opt_ad,
  beta_opt_ad,
  gamma_opt_ad
)

# Pronosticos
pronosticos_hw_mult <- modelo_holt_mult$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronósticos Holt Winters Multiplicativo:", round(pronosticos_hw_mult, 7), "\n"
)

# Ajustar Tendencia lineal
modelo_tendencia3 <- ajustar_tendencia(
  y = y3_est,
  tipo = "lineal",
  corregir_sesgo = FALSE
)

# Pronosticos
pronosticos_tendencia3 <- modelo_tendencia3$pronosticar(h_3)

cat(
  "Horizonte de validación (h):", h_3, "\n",
  "Pronósticos de tendencia lineal:",
  round(pronosticos_tendencia3, 4), "\n"
)

tabla_coef_lineal3 <- modelo_tendencia3$parametros$coeficientes

tabla_coef_lineal3$estimacion   <- round(tabla_coef_lineal3$estimacion, 4)
tabla_coef_lineal3$se_ordinario <- round(tabla_coef_lineal3$se_ordinario, 4)
tabla_coef_lineal3$se_robusto   <- round(tabla_coef_lineal3$se_robusto, 4)
tabla_coef_lineal3$t            <- round(tabla_coef_lineal3$t, 4)
tabla_coef_lineal3$valor_p      <- format.pval(tabla_coef_lineal3$valor_p, digits = 4, eps = 0.0001)

tabla_coef_lineal3

residuos_tl3 <- modelo_tendencia3$parametros$residuos

diagnostico_tendencia_lineal3 <- validar_errores(
  e = residuos_tl3,
  m = NULL,
  p = 0
)

diagnostico_tendencia_lineal3$correlograma$grafico

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_lineal3$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_lineal3$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_lineal3$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    diagnostico_tendencia_lineal3$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    diagnostico_tendencia_lineal3$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", diagnostico_tendencia_lineal3$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    diagnostico_tendencia_lineal3$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    diagnostico_tendencia_lineal3$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

diagnostico_tendencia_lineal3$durbin_watson <-
  durbin_watson(modelo_tendencia3$parametros$residuos)

cat(
  "Estadístico de prueba:",
  round(diagnostico_tendencia_lineal3$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(diagnostico_tendencia_lineal3$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(diagnostico_tendencia_lineal3$durbin_watson$dL, 4), "\n",
  "dU:",
  round(diagnostico_tendencia_lineal3$durbin_watson$dU, 4), "\n"
)

# Evaluacion de los metodos
eval_s3_media     <- origen_movil(serie3$y, fn_ajuste = ajustar_media, H = 7, n_eval = 40, s = 7)

eval_s3_ing_est   <- origen_movil(serie3$y, fn_ajuste = ajustar_ingenuo_estacional, H = 7, n_eval = 40, s = 7)

eval_s3_mm        <- origen_movil(serie3$y, fn_ajuste = ajustar_mm, H = 7, n_eval = 40, s = 7, k = resultado_opt_3$optimo$k)

eval_s3_ses       <- origen_movil(serie3$y, fn_ajuste = ajustar_ses, H = 7, n_eval = 40, s = 7, alpha = 
                                    resultado_opt_ses3$optimo$alpha)

eval_s3_tend_est  <- origen_movil(serie3$y, fn_ajuste = ajustar_tendencia_estacional, H = 7, n_eval = 40, s = 7)

eval_s3_hw_add <- origen_movil(
  serie3$y, 
  fn_ajuste = ajustar_hw, 
  H = 7, 
  n_eval = 40, 
  s = 7, 
  alpha = opt_holt_ad$optimo$alpha, 
  beta  = opt_holt_ad$optimo$beta, 
  gamma = opt_holt_ad$optimo$gamma
)

eval_s3_hw_mult <- origen_movil(
  serie3$y, 
  fn_ajuste = ajustar_hw_mult, 
  H = 7, 
  n_eval = 40, 
  s = 7, 
  alpha = opt_holt_ad$optimo$alpha, 
  beta  = opt_holt_ad$optimo$beta, 
  gamma = opt_holt_ad$optimo$gamma
)

eval_s3_lin_con   <- origen_movil(serie3$y, fn_ajuste = ajustar_tendencia, H = 7, n_eval = 40, s = 7, tipo = "lineal")

# Errores para h = 1
tabla_s3_h1 <- tibble(
  `Método de Pronóstico` = c(
    "Media Simple", 
    "Referente Ingenuo Estacional (s=7)", 
    "Media Móvil (MM)", 
    "Suavización Exponencial Simple (SES)", 
    "Tendencia Estacional (Ficticias)", 
    "Holt-Winters Aditivo", 
    "Holt-Winters Multiplicativo", 
    "Contraste: Tendencia Lineal MCO"
  ),
  RMSE = c(
    eval_s3_media$h1$RMSE, 
    eval_s3_ing_est$h1$RMSE, 
    eval_s3_mm$h1$RMSE, 
    eval_s3_ses$h1$RMSE, 
    eval_s3_tend_est$h1$RMSE, 
    eval_s3_hw_add$h1$RMSE, 
    eval_s3_hw_mult$h1$RMSE, 
    eval_s3_lin_con$h1$RMSE
  ),
  MAD = c(
    eval_s3_media$h1$MAD, 
    eval_s3_ing_est$h1$MAD, 
    eval_s3_mm$h1$MAD, 
    eval_s3_ses$h1$MAD, 
    eval_s3_tend_est$h1$MAD, 
    eval_s3_hw_add$h1$MAD, 
    eval_s3_hw_mult$h1$MAD, 
    eval_s3_lin_con$h1$MAD
  ),
  `MAPE (%)` = c(
    eval_s3_media$h1$MAPE, 
    eval_s3_ing_est$h1$MAPE, 
    eval_s3_mm$h1$MAPE, 
    eval_s3_ses$h1$MAPE, 
    eval_s3_tend_est$h1$MAPE, 
    eval_s3_hw_add$h1$MAPE, 
    eval_s3_hw_mult$h1$MAPE, 
    eval_s3_lin_con$h1$MAPE
  )
) %>% 
  arrange(RMSE) %>% 
  mutate(
    RMSE = round(RMSE, 3),
    MAD  = round(MAD, 3),
    `MAPE (%)` = round(`MAPE (%)`, 3)
  )

kable(
  tabla_s3_h1,
  align = "lccc",
  caption = "Serie 3: Medidas de error para horizonte h = 1 (Origen Móvil)",
  booktabs = TRUE
)

# Errores para h = 7
tabla_s3_h7 <- tibble(
  `Método de Pronóstico` = c(
    "Media Simple", 
    "Referente Ingenuo Estacional (s=7)", 
    "Media Móvil (MM)", 
    "Suavización Exponencial Simple (SES)", 
    "Tendencia Estacional (Ficticias)", 
    "Holt-Winters Aditivo", 
    "Holt-Winters Multiplicativo", 
    "Contraste: Tendencia Lineal MCO"
  ),
  RMSE = c(
    eval_s3_media$hH$RMSE, 
    eval_s3_ing_est$hH$RMSE, 
    eval_s3_mm$hH$RMSE, 
    eval_s3_ses$hH$RMSE, 
    eval_s3_tend_est$hH$RMSE, 
    eval_s3_hw_add$hH$RMSE, 
    eval_s3_hw_mult$hH$RMSE, 
    eval_s3_lin_con$hH$RMSE
  ),
  MAD = c(
    eval_s3_media$hH$MAD, 
    eval_s3_ing_est$hH$MAD, 
    eval_s3_mm$hH$MAD, 
    eval_s3_ses$hH$MAD, 
    eval_s3_tend_est$hH$MAD, 
    eval_s3_hw_add$hH$MAD, 
    eval_s3_hw_mult$hH$MAD, 
    eval_s3_lin_con$hH$MAD
  ),
  `MAPE (%)` = c(
    eval_s3_media$hH$MAPE, 
    eval_s3_ing_est$hH$MAPE, 
    eval_s3_mm$hH$MAPE, 
    eval_s3_ses$hH$MAPE, 
    eval_s3_tend_est$hH$MAPE, 
    eval_s3_hw_add$hH$MAPE, 
    eval_s3_hw_mult$hH$MAPE, 
    eval_s3_lin_con$hH$MAPE
  )
) %>% 
  arrange(RMSE) %>% 
  mutate(
    RMSE = round(RMSE, 3),
    MAD  = round(MAD, 3),
    `MAPE (%)` = round(`MAPE (%)`, 3)
  )

kable(
  tabla_s3_h7,
  align = "lccc",
  caption = "Serie 3: Medidas de error para horizonte h = 7 (Origen Móvil)",
  booktabs = TRUE
)

# Errores de Holt en origen móvil a un paso
errores_hw_ad <- eval_s3_hw_add$e_h1

# Validar los errores
resultado_errores_hw_ad <- validar_errores(
  e = errores_hw_ad,
  m = NULL,
  p = 0
)

g1_s3 <- resultado_errores_hw_ad$grafico_errores
g2_s3 <- resultado_errores_hw_ad$correlograma$grafico

g1_s3 | g2_s3

cat(
  "Estadístico de prueba:", round(resultado_errores_hw_ad$t, 4), "\n",
  "Grados de libertad:", resultado_errores_hw_ad$grados_libertad_t, "\n",
  "Valor crítico:", round(
    qt(0.975, resultado_errores_hw_ad$grados_libertad_t), 4
  ), "\n",
  "Valor p:", round(resultado_errores_hw_ad$valor_p_t, 4), "\n"
)

cat(
  "Estadístico de prueba:", round(
    resultado_errores_hw_ad$ljung_box$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_hw_ad$ljung_box$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_hw_ad$ljung_box$valor_critico, 4
  ), "\n",
  "Valor p:", round(
    resultado_errores_hw_ad$ljung_box$valor_p, 4
  ), "\n"
)

cat(
  "Estadístico de prueba:", round(
    resultado_errores_hw_ad$jarque_bera$estadistico, 4
  ), "\n",
  "Grados de libertad:", resultado_errores_hw_ad$jarque_bera$grados_libertad, "\n",
  "Valor crítico:", round(
    resultado_errores_hw_ad$jarque_bera$valor_critico, 4
  ), "\n",
  "Valor p:", format.pval(
    resultado_errores_hw_ad$jarque_bera$valor_p,
    digits = 4
  ), "\n"
)

cat(
  "Estadístico de prueba:",
  round(resultado_errores_hw_ad$durbin_watson$estadistico, 4), "\n",
  "d aprox:",
  round(resultado_errores_hw_ad$durbin_watson$d_aprox, 4), "\n",
  "dL:",
  round(resultado_errores_hw_ad$durbin_watson$dL, 4), "\n",
  "dU:",
  round(resultado_errores_hw_ad$durbin_watson$dU, 4), "\n"
)

T_3 <- length(serie3$y)
H_3 <- 7

# Ajustar Holt-Winters aditivo usando toda la serie 3
modelo_final_3 <- ajustar_hw(
  serie3$y,
  s = 7,
  alpha = alpha_opt_ad,
  beta = beta_opt_ad,
  gamma = gamma_opt_ad
)

# Pronósticos de los H días siguientes
pronosticos_finales_3 <- modelo_final_3$pronosticar(H_3)

# Desviación estándar de los errores para cada horizonte
desv_errores_3 <- sapply(1:7, function(h) {
  errores <- eval_s3_hw_add$todos$error[
    eval_s3_hw_add$todos$h == h
  ]
  sd(errores)
})

# Datos para el gráfico
grafico_final_3 <- data.frame(
  tiempo = 1:(T_3 + H_3),
  real = c(serie3$y, rep(NA, H_3)),
  ajuste = c(
    modelo_final_3$yhat,
    rep(NA, H_3)
  ),
  pronostico = c(
    rep(NA, T_3),
    pronosticos_finales_3
  ),
  inferior = c(
    rep(NA, T_3),
    pronosticos_finales_3 - 1.96 * desv_errores_3
  ),
  superior = c(
    rep(NA, T_3),
    pronosticos_finales_3 + 1.96 * desv_errores_3
  )
)

# Gráfico final
ggplot2::ggplot(
  grafico_final_3,
  ggplot2::aes(x = tiempo)
) +
  ggplot2::geom_line(
    ggplot2::aes(y = real),
    color = "purple"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ajuste),
    color = "green"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = pronostico),
    color = "green"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = inferior),
    color = "black",
    linetype = "dotted"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = superior),
    color = "black",
    linetype = "dotted"
  ) +
  ggplot2::geom_vline(
    xintercept = T_3,
    color = "red",
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Serie 3: ajuste y pronóstico final de Holt Winters Aditivo",
    subtitle = "Límites heurísticos de ±1,96 desviaciones estándar",
    x = "Fecha",
    y = "Ventas diarias (millones de pesos)"
  ) +
  ggplot2::theme_minimal()

# Ultimo tramo
ganador_s3 <- "Holt-Winters Aditivo"

# Se elimina el parámetro 'tipo = "aditivo"' que no existe en ajustar_hw()
fn_s3 <- function(y) {
  ajustar_hw(
    y, 
    s = 7, 
    alpha = opt_holt_ad$optimo$alpha,
    beta  = opt_holt_ad$optimo$beta,
    gamma = opt_holt_ad$optimo$gamma
  )
}

final_s3 <- pronostico_final(
  y = serie3$y, 
  fechas = serie3$fecha, 
  fn = fn_s3, 
  H = 7, 
  n_eval = 40, 
  paso = 1,
  titulo = paste0("Serie 3: pronóstico final a 7 días (", ganador_s3, ")"),
  unidad = attr(serie3, "unidad")
)

final_s3$grafico
tabla_final(final_s3, "Serie 3: Pronóstico final y banda heurística")

if (!dir.exists("figs")) dir.create("figs")

for (nombre in nombres_graficos) {
  grafico <- get(nombre)
  
  ggplot2::ggsave(
    filename = file.path("figs", paste0(nombre, ".png")),
    plot = grafico,
    width = 8,
    height = 6,
    dpi = 150
  )
}

cat("Gráficos guardados:", length(nombres_graficos), "\n")

# Guardar información de la sesión de R
writeLines(
  capture.output(sessionInfo()),
  "sesion-info.txt"
)

