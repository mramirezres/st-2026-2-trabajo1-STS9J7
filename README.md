Repositorio: https://github.com/mramirezres/st-2026-2-trabajo1-STS9J7 · Código: STS9J7

# Trabajo 1 — Series de tiempo

Departamento de Estadística, UNALMED. Pronosticamos tres series de tiempo (código **STS9J7**) para responder una pregunta de negocio en cada una: si se hace una promoción de 45 millones de pesos (Serie 1), si alcanza la capacidad de la licencia de 45.000 clientes (Serie 2) y cuánto pedir de frutas, verduras y panadería en la próxima semana (Serie 3).

## Integrantes y qué hizo cada una

| Integrante | Qué hizo |
| :--- | :--- |
| **Melissa Montoya Urrego** | Hizo el trabajo hasta el punto 5 (dependencia y estacionariedad). Al informe final aportó los gráficos que sirvieron para verificar que las raíces quedaran dentro del círculo unitario y una parte de los puntos del operador de rezago. |
| **Isabela Pulgarín Durango** | Hizo el trabajo completo. Su código es la base del informe: de ahí salen la lectura de las series, los gráficos, el correlograma, la prueba de Ljung–Box y los métodos de pronóstico. |
| **Manuela Ramírez Restrepo** | Hizo el trabajo completo. De su código se usó toda la parte de medidas de error, por lo que la función `origen_movil()` es suya, al igual que el pronóstico final y la función para graficarlo (`pronostico_final()`, `tabla_final()` y la gráfica del pronóstico). También escribió la sección «Argumentos para los interesados», juntó los códigos de las tres en un solo informe y hizo la demostración a mano del punto 4. |

**Cómo trabajamos juntas.** Cada una hizo el trabajo por su cuenta. Después nos reunimos a comparar los resultados que nos dio a cada una y verificamos que todo estuviera correcto. Cuando algo no coincidía, lo revisamos hasta saber cuál era la versión correcta y esa fue la que quedó en el informe.

## Qué contiene el repositorio

| Archivo | Contenido | Autora |
| :--- | :--- | :--- |
| `README.md` | Este archivo. | Las tres |
| `st-2026-2-trabajo1-STS9J7.Rproj` | Proyecto de RStudio. Se abre con doble clic para que R tome la carpeta del repositorio como carpeta de trabajo. | — |
| `.gitignore` | Archivos que Git no sube (por ejemplo, los temporales de R). | — |
| `datos/STS9J7-serie-1.csv` | Serie 1: valor promedio semanal de la compra (miles de pesos), 200 semanas. | Entregado con el trabajo |
| `datos/STS9J7-serie-2.csv` | Serie 2: clientes registrados por semana (cientos de clientes), 180 semanas. | Entregado con el trabajo |
| `datos/STS9J7-serie-3.csv` | Serie 3: ventas diarias del supermercado (millones de pesos), 196 días. | Entregado con el trabajo |
| `R/00-lectura.R` | `leer_serie()`: lee un archivo CSV y revisa que las fechas y los valores estén bien. | Isabela |
| `R/01-graficos.R` | `graficar_serie()` y `correlograma()` (ACF y PACF calculadas a mano), y los gráficos de la optimización, de Holt y de Holt-Winters. | Isabela |
| `R/02-operadores.R` | Operador de rezago: polinomios de las diferencias, cálculo de sus raíces y comprobación de que queden dentro del círculo unitario. | Melissa e Isabela *(confirmar)* |
| `R/03-dependencia.R` | Pruebas de dependencia: `ljung_box()` y `durbin_watson()`. | Isabela |
| `R/04-metodos.R` | Métodos de pronóstico (`ajustar_media()`, `ajustar_ingenuo()`, `ajustar_ingenuo_estacional()`, `ajustar_mm()`, `ajustar_dmm()`, `ajustar_ses()`, `ajustar_tendencia()`, `ajustar_tendencia_estacional()`, `ajustar_holt()`, `ajustar_hw()`, `ajustar_hw_mult()`), `resumen_regresion()` y la búsqueda de parámetros. | Isabela *(confirmar)* |
| `R/05-evaluacion.R` | `medidas()`, `origen_movil()`, `validar_errores()`, `pronostico_final()` y `tabla_final()`. | Manuela |
| `informe/informe.qmd` | Informe completo en Quarto: descripción de las series, operador de rezago, dependencia, pronóstico y evaluación, y argumentos para los interesados. Junta el trabajo de las tres. | Las tres (base de Isabela; unión de Manuela) |
| `informe/demostrar4.bib` | Bibliografía del informe. | Manuela |
| `informe/informe.html` | Informe ya generado, para leerlo sin correr nada. | Las tres |
| `analisis/` | carga R/ y corre las tres series en orden *[correr-todo.R]*. | Isabela |

### Archivos reutilizados y modificaciones

Reutilizamos funciones de la Tarea 1 de las integrantes, corregidas cuando hizo falta. Aquí está de quién es cada archivo y qué se le cambió.

**`R/00-lectura.R` — `leer_serie()`.** Responsable: Isabela Pulgarín. Se reutilizó la función de la Tarea 1 y se adaptó a este trabajo.
- Se quitó el manejo de objetos `ts`, porque aquí las series llegan directamente en archivos CSV.
- Ahora reconoce las dos frecuencias de las series entregadas: diaria y semanal.
- Se agregó la revisión de que las fechas tengan una frecuencia constante y no tengan huecos.
- Se mantuvo la revisión de las columnas `fecha` y `valor`, de fechas inválidas, valores faltantes, orden de las fechas y número mínimo de observaciones.
- Se conservaron los atributos `fuente` y `unidad`.
- Se mantuvo la salida como una tabla con las columnas `t`, `fecha` y `y`.

**`R/01-graficos.R` — `graficar_serie()`.** Responsable: Isabela Pulgarín. Se reutilizó tal como estaba en la Tarea 1. No se cambió nada, porque ya funciona con las series de este trabajo y con `leer_serie()`.

**`R/01-graficos.R` — `correlograma()`.** Responsable: Isabela Pulgarín. Se reutilizó la función de la Tarea 1.
- Se mantuvo el cálculo manual de la ACF.
- Se cambió el cálculo de la PACF: ahora se hace dentro de la función, sin usar `pacf()`.
- Se mantuvo el gráfico conjunto de ACF y PACF, con sus bandas de confianza y el color morado.

**`R/03-dependencia.R` — `ljung_box()`.** Responsable: Isabela Pulgarín. Se reutilizó la función de la Tarea 1 y se adaptó a este trabajo.
- Se mantuvo el cálculo manual del estadístico de Ljung–Box, de los grados de libertad, del valor crítico y del valor p.
- Se agregaron `m` y `p` a la salida, para completar los seis elementos que se piden.
- Se mantuvieron las validaciones de los datos y de los parámetros.

**`R/04-metodos.R` — métodos de pronóstico.** Responsable: *Isabela*. Cambios:
- `ajustar_media()`: se corrigió el uso de `T` al calcular las predicciones dentro de la serie y se agregó una validación del horizonte `h`.
- `ajustar_mm()`: ahora acepta ventanas de tamaño `k = 1`, se corrigió el cálculo de los pronósticos y se cubrió el caso en que `k` es igual al número de observaciones.
- `ajustar_dmm()`: se corrigió el manejo del número de observaciones y se agregaron validaciones para que haya datos suficientes según el tamaño de la ventana.
- `ajustar_ses()`: la validación de `alpha` ahora acepta valores en `(0, 1]` y se mejoró la validación del horizonte.
- `ajustar_holt()`: se unificó la validación de `alpha` y `beta` para aceptar valores en `(0, 1]` y se corrigió el manejo del número de observaciones.
- `ajustar_tendencia()`: se reorganizó la función para corregir errores en las condiciones y en los bloques de código, y se ajustaron los cálculos y la información que devuelve para las tendencias lineal, cuadrática y exponencial.
- `resumen_regresion()`: se corrigió el cálculo de los grados de libertad y su uso en los valores p robustos, y se agregaron validaciones de las dimensiones y los valores de entrada.
- Constantes de suavizamiento: se fijó una rejilla común `seq(0.05, 1, by = 0.05)` para SES, Holt, Holt-Winters aditivo y Holt-Winters multiplicativo.

**`R/05-evaluacion.R` — `medidas()` y `origen_movil()`.** Responsable: Manuela Ramírez. Se reutilizó su parte de medidas de error.
- `medidas()`: el MASE ahora usa como escala los errores del método ingenuo en el tramo de estimación. Para eso se agregó el argumento `y_estimacion` y se reforzaron las validaciones de los datos y del período estacional `s`.
- `origen_movil()`, `pronostico_final()` y `tabla_final()` salen de su código. 

**Gráficos de las raíces y puntos del operador de rezago.** Responsable: Melissa Montoya. Se usaron sus gráficos para comprobar que las raíces quedaran dentro del círculo unitario y parte de sus puntos del operador de rezago.

## Cómo se corre

**Qué se necesita:** R *4.5.2*, [Quarto](https://quarto.org/docs/get-started/) y estos paquetes de R: `ggplot2`, `patchwork`, `dplyr`, `tibble`, `knitr` y `rmarkdown`.

**Paso a paso.** Abre una terminal (en RStudio, la pestaña *Terminal*) y ejecuta, en este orden:

```bash
git clone https://github.com/mramirezres/st-2026-2-trabajo1-STS9J7.git
cd st-2026-2-trabajo1-STS9J7
Rscript -e 'install.packages(c("ggplot2", "patchwork", "dplyr", "tibble", "knitr", "rmarkdown"))'
quarto render informe/informe.qmd
```

El último comando lee las tres series de `datos/`, carga las funciones de `R/`, corre todo el análisis y genera `informe/informe.html`, que es un solo archivo con todo incluido. Ábrelo con el navegador.

**Desde RStudio.** Abre `st-2026-2-trabajo1-STS9J7.Rproj`, abre `informe/informe.qmd` y pulsa **Render**.

**Importante.** Las rutas son relativas a la carpeta `informe/` (el informe usa `ruta <- ".."`), así que no hay que cambiar nada para correrlo en otro computador. Hay que renderizarlo desde el mismo repositorio, sin mover las carpetas `R/` y `datos/`.

**Tiempo de ejecución observado:** *3 minutos]*. Lo medimos con:


## Resumen de resultados

Para cada serie se eligió el método con el menor error de pronóstico en el origen móvil. El referente es el método ingenuo (en la Serie 3, el ingenuo estacional). El RMSE está en la unidad de cada serie: miles de pesos (Serie 1), cientos de clientes (Serie 2) y millones de pesos (Serie 3). *h* = 1 es el pronóstico a un paso y *H* es el horizonte completo de cada serie.

| Serie | Transformación elegida | Ganador | Parámetros | RMSE en *h* = 1 (ganador vs. referente) | RMSE en *H* (ganador vs. referente) |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **1.** Valor promedio semanal de la compra | Primera diferencia, (1 − B) | Holt | α = 0.3, β = 0.05 | 12.03 vs. 12.62 | *H* = 4: 15.35 vs. 18.12 |
| **2.** Clientes registrados por semana | Primera diferencia, (1 − B) | En *h* = 1: Holt. En *H* = 6: tendencia exponencial | Holt: α = 0.3, β = 0.05. Exponencial: log *y* = 5.7376 + 0.0018·*t* | 4.17 vs. 4.38 | *H* = 6: 4.26 (exponencial) vs. 7.00. Holt: 5.57 |
| **3.** Ventas diarias del supermercado | Diferencia estacional, (1 − B⁷) | Holt-Winters aditivo | α = 0.3, β = 0.2, γ = 0.2 | 10.89 vs. 11.94 | *H* = 7: 11.79 vs. *[completar: RMSE del ingenuo estacional en H = 7]* |

**Respuestas a las preguntas del negocio** (están explicadas en el informe):
- **Serie 1:** el valor promedio de la compra se pronostica entre 559.57 y 562.29 miles de pesos en las próximas 4 semanas. Ni el límite inferior más bajo de la banda (531.79) baja de la meta de 510, así que no hace falta la promoción de 45 millones.
- **Serie 2:** se esperan unos 44.088 clientes en la semana 6, frente a una capacidad de 45.000 (margen de unos 912 clientes). Al ritmo actual (unos 77 clientes por semana) se llegaría a 45.000 hacia la semana 18, y como la ampliación tarda 6 semanas en activarse, la compra se decide a más tardar hacia la semana 12.
- **Serie 3:** el pronóstico de lunes a domingo va de 398.19 millones (martes) a 482.99 millones (sábado), con un total semanal de 2,970.96 millones. Conviene pedir según el día y no con un promedio único.

## Declaración de uso de IA

Usamos un asistente de IA (Claude, de Anthropic) como apoyo. 

**Lo que se pidió**
1. Cómo subir commits a un repositorio de GitHub que creó una compañera, cómo clonarlo desde RStudio y cómo enlazar GitHub con R (token, configuración de nombre y correo).
2. Ayuda con errores al clonar (la URL terminaba en `/tree/main`, la carpeta de destino ya existía y OneDrive no dejaba borrarla) y cómo mover o eliminar un archivo del repositorio.
3. Que, mientras se hacía la unión de los códigos, se fuera verificando que las variables se llamaran igual que en el archivo base, para que el trabajo pudiera funcionar.
4. Hacer las siete diapositivas del trabajo a partir del informe.


**Lo que se recibió**
1. Los pasos para clonar, configurar la identidad, hacer pull, commit y push, y la explicación de los errores.
2. Una verificación de que todo tuviera coherencia, ya que al unir los códigos podían generarse equivocaciones.
3. Las diapositivas.


**Lo que verificamos por cuenta propia** *(ajustar a lo que realmente hicimos)*
- Al unir los códigos fuimos comprobando que las variables se llamaran igual que en el archivo base, para que el trabajo funcionara.
- Revisamos cada cifra de las diapositivas y de la sección de argumentos contra los resultados que imprime el informe.
- Comprobamos que el informe se renderiza desde cero en un computador distinto al de quien lo escribió, con las rutas relativas.
- Cualquiera de las tres puede explicar cualquier línea del repositorio.