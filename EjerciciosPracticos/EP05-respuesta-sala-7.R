# EP-05 Sala 7
# Arturo Cadenas (20.468.370-0)
# Stephan Silva (20.298.778-8)
# Ivan Zuñiga (20.003.345-0)

library(ggpubr)
library(pwr)
library (tidyverse)

# Se sabe que el proceso de fabricación de barras de acero para concreto 
# reforzado producen barras con medidas de dureza que siguen una distribución
# normal con desviación estándar de 10 kilogramos de fuerza por milímetro
# cuadrado. Usando una muestra aleatoria de tamaño 25, un ingeniero quiere 
# averiguar si una línea de producción está generando barras con dureza media
# de 170 [kgf mm^-2]

# Pregunta 1

# Si el ingeniero está seguro que la verdadera dureza media no puede ser menor
# a los 170 [kgf mm^-2] y piensa rechazar la hipótesis nula cuando la muestra
# presente una media mayor a 174 [kgf mm^-2], ¿cuál es la probabilidad de que
# cometa un error de tipo 1?

# Se pretende calcular el valor de alpha pues representa la probabilidad de
# cometer el error de tipo 1.

# Fijar valores conocidos.
sigma <- 10
n <- 25
valor_nulo <- 170

# Calcular el error estándar.
SE <- sigma/sqrt(n)

# Gráficar la distribución muestral de la media de las diferencias si
# la hipótesis nula fuera verdadera.
# Se genera la secuencia.
x <- seq( 80 * SE , 90 * SE , 0.01)
# Se genera la distribución.
y <- dnorm(x , mean = valor_nulo , sd = SE )
# Se procede con el gráfico.
g <- ggplot( data = data.frame(x, y), aes(x))
g <- g + stat_function(
  fun = dnorm,
  args = list( mean = valor_nulo, sd = SE ),
  colour = "red", size = 1)
g <- g + ylab ("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Dureza media de las barras [kgf mm^-2]",
                            breaks = seq( 80 * SE , 90 * SE , 2))
g <- g + theme_pubr()

# Colorear la región de rechazo de la hipótesis nula.
# Caso unilateral, se rechaza la parte superior sobre 174.
critico_superior <- 174
g <- g + geom_area( data = subset(g$data, x > critico_superior),
                       aes ( y = y ) ,
                       colour = "red ",
                       fill = "red",
                       alpha = 0.5)
print(g)


# Cálculo teórico de alpha, con respecto a la proporción de observaciones
# que se encuentran en la zona de rechazo, y al total de observaciones.
cantidad_rechaza <- length(subset(g$data, x > critico_superior)$x)
alpha <- cantidad_rechaza / length(x)


# Resultado:
# La probabilidad de que se cometa un error de tipo 1, si se piensa rechazar la
# hipótesis nula cuando la muestra presente una media mayor a 174 [kgf mm^-2]
# es de 0,2999%.
#-------------------------------------------------------------------------------

# Pregunta 2

# Si la verdadera dureza media de la línea de producción fuera 173 [kgf mm^-2],
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce
# este dato, cometa un error de tipo 2?

# Se pretende calcular el valor de beta, pues este corresponde a la probabilidad
# de cometer error de tipo 2, calculándolo como 1 menos el poder.

# Nuevo valor conocido.
media_verdadera <- 173
# Calcular el poder de acuerdo al análisis teórico .
poder <- pnorm(174,
               mean = media_verdadera,
               sd = SE,
               lower.tail = FALSE)

beta <- 1 - poder
cat ("Beta = ", beta , "\n")

# Resultado:
# Considerando que la verdadera dureza media de la línea de producción fuera 
# 173 [kgf mm^-2], la probabilidad de que el ingeniero cometa un error de tipo 2
# es de 0,6915%.
#-------------------------------------------------------------------------------

# Pregunta 3

# Como no se conoce la verdadera dureza media, genere un gráfico del poder
# estadístico con las condiciones anteriores, pero suponiendo que las verdaderas
# durezas medias podrían variar de 170 a 178 [kgf mm^-2].

# Se genera la secuencia para variar el tamaño de 170 a 178. 
efecto <- seq(0, 8 , 0.01)

# Calcular el poder para una prueba unilateral , para cada tamaño
# del efecto , con desviación estándar igual a 10.

caso <- power.t.test( n = n ,
                       delta = efecto,
                       sd = sigma,
                       sig.level = alpha ,
                       type = "one.sample",
                       alternative = "one.sided")$power

# Se guardan los datos en un dataframe.
datos <- data.frame(efecto,caso)

# Se procede a graficar poder vs tamaño efecto.
g1 <- ggplot(datos, aes(efecto, caso))
g1 <- g1 + geom_line ()
g1 <- g1 + ylab ("Poder estadístico ")
g1 <- g1 + xlab ("Tamaño del efecto ")
g1 <- g1 + theme_pubr()
g1 <- g1 + ggtitle("Poder estadístico prueba unilateral con dureza variando")
print(g1)



#-------------------------------------------------------------------------------

# Pregunta 4

# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,85
# y un nivel de significación de 0,05?

# Se pretende obtener el poder usando la función power.t.test

# Se inicializan los valores entregados
poder <- 0.85
diferencia <- media_verdadera - valor_nulo
alfa <- 0.05
desv_est <- 10

# Se calcula la cantidad de barras necesarias utilizando power.t.test
resultado <- power.t.test(n = NULL,
                          delta = diferencia,
                          sd = desv_est,
                          sig.level = alfa,
                          power = poder,
                          type = "one.sample",
                          alternative = "one.sided")
print(resultado)

# Resultado:
# Con respecto al resultado entregado por la función power.t.test, la cantidad
# de barras que se deberían revisar para conseguir un poder estadístico de 0,85
# y un nivel de significación de 0,05 es de 82 barras.
#-------------------------------------------------------------------------------

# Pregunta 5

# ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error
# de tipo 1 a un 1% solamente?

# De la misma forma se pretende obtener el poder usando la función power.t.test.

# Se inicializa el nuevo valor de alfa.
alfa <- 0.01
# Se calcula la cantidad de barras necesarias utilizando power.t.test
resultado2 <- power.t.test ( n = NULL ,
                            delta = diferencia ,
                            sd = desv_est ,
                            sig.level = alfa ,
                            power = poder ,
                            type = "one.sample",
                            alternative = "one.sided")
print(resultado2)

# Resultado:
# Con respecto al resultado entregado por la función power.t.test, la cantidad
# de barras que se deberían revisar para conseguir un poder estadístico de 0,85,
# siendo exigente y bajando la probabilidad de cometer un error de tipo 1 a un
# 1% (nivel de significación de 0,01), es de 129 barras.
#-------------------------------------------------------------------------------
