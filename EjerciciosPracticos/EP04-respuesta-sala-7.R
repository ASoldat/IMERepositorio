# EP-04 Sala 7
# Arturo Cadenas (20.468.370-0)
# Stephan Silva (20.298.778-8)

# Se importan librerías

library(ggpubr)
library(dplyr)

# Pregunta 1

# El artículo “Engineering Properties of Soil” (Soil Science 1998) puso a prueba la idea generalizada de que
# la materia orgánica en el suelo no supera el 3%. Para esto, los autores obtuvieron una muestra aleatoria
# de especímenes de suelo, determinando que el porcentaje de materia orgánica presente en cada
# espécimen era (usando punto en vez de coma decimal):

#     3.10 5.09 2.97 1.59 4.60 3.32 0.55 1.45 0.14 4.47
#     0.80 3.50 5.02 4.67 5.22 2.69 3.98 3.17 3.03 2.21
#     2.69 4.47 3.31 1.17 2.76 1.17 1.57 2.62 1.66 2.05

# ¿Qué conclusión sugeriría a los autores?

# Formulación de hipótesis:
# H0: El porcentaje de materia orgánica en el suelo es igual al 3%. (mu = 3)
# HA: El porcentaje de materia orgánica presente en el suelo es menor al 3%. (mu < 3)

# Cargar los datos.

texto1 <- "3.10 5.09 2.97 1.59 4.60 3.32 0.55 1.45 0.14 4.47
           0.80 3.50 5.02 4.67 5.22 2.69 3.98 3.17 3.03 2.21
           2.69 4.47 3.31 1.17 2.76 1.17 1.57 2.62 1.66 2.05"

file1 <- textConnection(texto1)
materia_organica <- scan(file1)

# Se eligió la prueba t-student para una muestra simple, ya que el tamaño de la muestra es de 30 observaciones y no
# se tiene el dato de la varianza poblacional.
# Por lo que se debe inicialmente verificar si las observaciones provengan de una distribución cercana a la normal y
# que las observaciones sean independientes entre si.

# Establecer los datos conocidos.
n <- length(materia_organica)
grados_libertad <- n - 1
valor_nulo <- 3


# Verificar si la distribución se acerca a la normal.
g <- ggqqplot(data = data.frame(materia_organica),
              x = "materia_organica",
              color = "steelblue",
              xlab = "Teórico",
              ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr.normal")

# En el gráfico Q-Q, los valores de la muestra se encuentran dentro del margen
# aceptable, por lo cual se puede concluir que existe normalidad dentro del conjunto de datos dado.
# Y en conclusión, se cumple esta condición para poder aplicar la prueba t-student.
# Ademas, se dice que las muestras son independientes entre si, ya que las observaciones fueron
# obtenidas mediante una muestreo aleatorio. 

print(g)

# Fijar un nivel de significación.
alfa <- 0.05

# Calcular el estadístico de prueba .
cat("\tPrueba t para una muestra\n\n")
media <- mean(materia_organica)
cat ("Media = ", media, "%\n")
desv_est <- sd(materia_organica)
error <- desv_est / sqrt(n)
t <- (media - valor_nulo) / error
cat("t =", t, "\n")

# Calcular el valor p.
p <- pt(t, df = grados_libertad, lower.tail = TRUE)
cat ("p = ", p , "\n")

# Construir el intervalo de confianza .
t_critico <- qt(alfa, df = grados_libertad, lower.tail = FALSE)
superior <- media + t_critico * error
cat ("Intervalo de confianza = (-Inf, ", superior, "]\n", sep = "")

# Aplicar la prueba t de Student con la función de R.
prueba <- t.test(materia_organica,
                 alternative = "less",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

print(prueba)

# A partir de los resultados podemos observar que el valor p obtenido es grande 
# y mayor al nivel de significación (0,266 > 0,05), 
# En consecuencia, se falla al rechazar H0 en favor de HA. 
# Se puede afirmar, con 95% de confianza, que la materia orgánica en el suelo es de un 3%.

# Pregunta 2.

# Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar cantidades de calcio
# adecuadas para la producción de leche. Un estudio intentó determinar si madres adolescentes podían
# recuperar niveles más normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-
# 1326). El estudio obtuvo las siguientes medidas del contenido total de minerales en los huesos del cuerpo
# (en gramos) para una muestra de madres adolescentes tanto durante la lactancia (6-24 semanas
# postparto) y posterior a ella (12-30 semana postparto):

# ¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por no más de 50 g?

# Se plantean hipótesis:
# H0: La media de las diferencias entre lactancia y posdestete es 50g.(mu_dif=50)
# HA: La media de las diferencias entre lactancia y posdestete es menor a 50g. (mu_dif<50)

# Cargar los datos.

texto2 <- "1928 2549 2825 1924 1628 2175 2114 2621 1843 2541"

file2 <- textConnection(texto2)
lactancia <- scan(file2)

texto3 <- "1986 2745 2755 1802 1610 2044 2024 2486 1866 2487"

file3 <- textConnection(texto3)
posdestete <- scan(file3)

diferencia <- lactancia - posdestete

# Se eligió la prueba t-student para muestras pareadas, ya que el tamaño de la muestra en ambas observaciones
# es menor a 30 y no se tiene el dato de la varianza poblacional.
# Por lo que se debe inicialmente verificar si las observaciones provengan de una distribución cercana a la normal y
# que las observaciones sean independientes entre si.

# Verificar si la distribución se acerca a la normal mediante la prueba de Shapiro-Wilk.
normalidad <- shapiro.test(diferencia)
print(normalidad)

# Al aplicar una prueba de normalidad de Shapiro-Wilk se obtiene p = 0,139 ,
# con lo que podemos concluir que la diferencia de medias entre lactancia y posdestete se acerca razonablemente a una
# distribución normal. En consecuencia, podemos proceder con la prueba t de Student.

# Fijar un nivel de significación.
alfa <- 0.05

# Fijar valor nulo
valor_nulo <- 50

# Se aplica la prueba t de Student para dos muestras pareadas.

prueba_1 <- t.test(x = lactancia,
                   y = posdestete,
                   paired = TRUE,
                   alternative = "less",
                   mu = valor_nulo,
                   conf.level = 1 - alfa)

print(prueba_1)


# La media de las diferencias esta dentro del intervalo de confianza, y el Valor de p es mayor que el nivel de significación.
# En consecuencia, se falla en rechazar la hipótesis nula. Ademas, con un 95% de confianza, 
# el excedente promedio de minerales en los huesos será igual a 50g. Se necesitaría una muestra mas grande
# para tener mayor certeza, ya que una muestra de 10 datos no es significativa.
# Por lo tanto, los datos no sugieren que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por no más de 50 g.

# Pregunta 3

# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 7ma
# región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en linaza (linseed) y el basado en soya (soybean).

# Se declaran las hipótesis:
# H0: No hay diferencia entre la efectividad promedio entre ambos suplementos. (mu_A = mu_B)
# HA: Existe diferencia entre la efectividad promedio entre ambos suplementos. (mu_A != mu_B)

# Cargar los datos.
datos <- chickwts

# Se filtran los datos referentes a linseed
datosLinseed <- datos %>% filter(feed == "linseed")

# Se elige la columna weight con los datos deseados
suplemento_A <- datosLinseed$weight

# Se filtran los datos referentes a soybean
datosSoybean <- datos %>% filter(feed == "soybean")

# Se elige la columna weight con los datos deseados
suplemento_B <- datosSoybean$weight

# Se eligió la prueba t-student para muestras independientes, ya que el tamaño de las muestras en ambas observaciones
# es menor a 30 y no se tiene el dato de la varianza poblacional.
# Por lo que se debe inicialmente verificar si las observaciones provengan de una distribución cercana a la normal y
# que las observaciones sean independientes entre si.

# Se verifica si las muestras se distribuyen de manera cercana
# a la normal aplicando la prueba de Shapiro-Wilk a ambas muestras.

normalidad_A <- shapiro.test(suplemento_A)
print(normalidad_A)

normalidad_B <- shapiro.test(suplemento_B)
print(normalidad_B)

# Los valores de p para ambas muestras son altos (p_A = 0.904 y p_B = 0.506), por lo que se puede concluir
# que ambas muestras provienen de poblaciones que se distribuyen de 
# forma aproximadamente normal.

# Fijar un nivel de significación.
alfa <- 0.05

# Aplicar la prueba t para dos muestras independientes.
prueba <- t.test(x = suplemento_A,
                 y = suplemento_B,
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba)

# Calcular la diferencia entre las medias.

media_A <- mean(suplemento_A)
media_B <- mean(suplemento_B)
diferencia <- media_A - media_B
cat("Diferencia de las medias = ", diferencia, " [g]\n")

# Al aplicar la prueba t, obtenemos que la diferencia entre las medias es -27,679 [g] y que el
# intervalo de confianza es [-70,843; 15,485). Además, el valor p es 0,198, superior al nivel de significación
# el cual es 0,05. Esto significa que la evidencia en favor de HA es muy débil, 
# por lo que se falla en rechazar la hipótesis nula.
# En consecuencia, podemos concluir con 95% de confianza que 
# no existe diferencias de efectividad promedio entre ambos suplementos.