# EP-07
# Inf. y Modelos Estadísticos
# Arturo Cadenas (20.468.370-0)
# Stephan Silva (20.298.778-8)
# Ivan Zuñiga (20.003.345-0)

# Pregunta 1

# Se cree que la ardilla chilena (Octodon degus) emite un chillido cuando es
# perseguido por un depredador,posiblemente para alertar a otros degús. Se hizo
# un experimento en que 45 degús se liberaron a 10 o a # 100 metros de su
# madriguera y luego se les perseguía hasta que se metían en ella, para simular
# la persecución de un depredador. De las 24 ardillas liberadas a 10 metros de
# la madriguera, 15 emitieron el chillido esperado, mientras que 8 de las 21
# ardillas liberadas a 100 metros de la madriguera lo hicieron.
# ¿Influye la distancia a la madriguera en la emisión del chillido por parte
# de un degú?

# H0: La distancia a la madriguera no influye en la emisión del chillido por 
#     parte de un degú 
# HA: La distancia a la madriguera influye en la emisión del chillido por 
#     parte de un degú 



# Crear tabla de contingencia .
chilla <- c(15 , 8)
no_chilla <- c(9, 13)
tabla <- as.table(rbind(chilla, no_chilla))
dimnames(tabla) <- list(Sonido = c("Chilla", "No Chilla"),
                        Distancia = c("10 metros", "100 metros"))
print(tabla)

# Aplicar prueba exacta de Fisher, ya que ambas variables cumplen la condición
# de ser dicotómicas.
# Podemos suponer que la muestra fue obtenida de manera aleatoria, y 
# representa menos del 10 % de la población mundial de ardillas chilenas. 
# En consecuencia, se verifica la condición de independencia de las 
# observaciones en las muestras.

alfa <- 0.05
prueba <- fisher.test(tabla, 1 - alfa)

print(prueba)

# Respuesta:
# Con respecto a la prueba realizada y al valor p obtenido de 0.1392, este es
# mayor al valor de alfa de 0.05 (respecto al nivel de significación), por lo 
# que no hay evidencia suficiente para rechazar la hipótesis nula (se falla al 
# rechazar) de esta manera se puede concluir con un 95% de confianza que la 
# distancia a la madriguera no influye en la emisión del chillido por parte de un degú.

#-------------------------------------------------------------------------------

# Pregunta 2

# Un artículo describe un estudio en que se compararon diferentes versiones de
# algoritmos evolutivos para resolver variadas instancias de problemas de
# clasificación tomadas desde el UCI Machine Learning Repository. La siguiente
# tabla muestra los resultados de la clasificación 
# (COR: correcta, INC: incorrecta) hecha por dos versiones de un algoritmo
# genético evaluado en el estudio para el problema Breast Cancer.
# ¿Hay un algoritmo con mejor desempeño que el otro?

# H0: No hay diferencias significativas en el desempeño de los algoritmos.
# HA: Si hay diferencias significativas en el desempeño de los algoritmos.

# Podemos suponer que la muestra fue obtenida de manera aleatoria, ya que se 
# trata de un estudio científico. En consecuencia, se verifica la condición de 
# independencia de las observaciones en las muestras.

# Se aplicará la prueba de mcNemar, ya que en diferentes ocasiones, se mide con
# respuesta dicotómica a los mismos sujetos y medir la existencia de 
# diferencias significativas. En este caso, hablando de algoritmos, medir 
# si hay una diferencia significativa en el desempeño de estos.

algoritmo <- seq(1:14)

modelo_1 <- c(rep("Incorrecto", 3), 
              rep("Correcto", 1), 
              rep("Incorrecto", 3), 
              rep("Correcto", 1), 
              rep("Incorrecto", 3), 
              rep("Correcto", 1), 
              rep("Incorrecto", 2))

modelo_2 <- c(rep("Correcto", 5), 
              rep("Incorrecto", 1), 
              rep("Correcto", 5), 
              rep("Incorrecto", 1), 
              rep("Correcto", 2))

datos <- data.frame(algoritmo, modelo_2, modelo_1)
tabla2 <- table( modelo_2, modelo_1)
print(tabla2)
prueba2 <- mcnemar.test(tabla2)
print(prueba2)

# Resultado:
# Con respecto a la prueba realizada y al valor p obtenido de 0.01586, este es
# menor al valor de alfa de 0.05 (respecto al nivel de significación), por lo
# que se rechaza la hipótesis nula a favor de la hipótesis alternativa, de esta
# manera se concluye con 95% de confianza que si hay diferencias significativas 
# en el desempeño de los algoritmos.

#-------------------------------------------------------------------------------

# Pregunta 3

# Una investigación monitoreó a más de 50 mil mujeres adultas durante 10 años 
# (Lucas et al., 2011. Coffee, Caffeine, and Risk of Depression Among Women. 
# Archives of Internal Medicine, 171(17), 1571–1578) con la intención de 
# identificar factores de riesgo de desarrollar un cuadro de depresión. 
# Entre otras cosas, se registró el consumo de cafeína, cuyos datos se resumen
# en la siguiente tabla. ¿Existe alguna asociación entre la cantidad de café 
# que se bebe y la depresión?

# H0: Las variables cantidad de café que se bebe y la depresión son independientes
# HA: Las variables cantidad de café que se bebe y la depresión están relacionadas

# Crear tabla de contingencia.

d_si <- c(640, 353, 905, 584, 110)
d_no <- c(11575, 6264, 16329, 11706, 2273)

tabla3 <- as.table(rbind(d_si , d_no))

dimnames(tabla3) <- list(Depresion = c("Sí", "No"),
                         consumo = c("<= 1 taza por semana",
                                     "2-6 tazas por semana", 
                                     "1 taza al día", 
                                     "2-3 tazas al día",
                                     ">= 4 tazas al día"))

print(tabla3)

# Hacer prueba chi - cuadrado de independencia.

prueba3 <- chisq.test(tabla3)
cat("\nLa prueba internamente calcula los valores esperados :\n")
esperados <- round(prueba3[["expected"]], 3)
print(esperados)

# Se obtienen los valores esperados que se presentan en la tabla. Podemos 
# ver que todos los valores esperados superan las 5 observaciones,y se supone
# además que la muestra fue obtenida de manera aleatoria, y esta 
# representa menos del 10 % de la población, por lo que 
# podemos proceder con la prueba chi-cuadrado de independencia.

cat("\nResultado de la prueba :\n")
print(prueba3)

# Respuesta:
# Con respecto a la prueba realizada y al valor p obtenido de 0.1797, este es
# mayor al valor de alfa de 0.05 (respecto al nivel de significación), por lo 
# que no hay evidencia suficiente para rechazar la hipótesis nula (se falla al rechazar)
# de esta manera se puede concluir con un 95% de confianza que Las variables 
# cantidad de café que se bebe y la depresión son independientes.

#-------------------------------------------------------------------------------

# Pregunta 4

# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) 
# relacionado con el ejercicio regular que realizan los chilenos antes y 
# después de la pandemia de COVID19, que requiera utilizar una prueba Q
# de Cochran. Identifique las variables involucradas y las hipótesis a 
# contrastar.

# Contexto:
# Luego de 2 años iniciada la pandemia y el confinamiento, se realizó un estudio 
# para analizar la cantidad de ejercicio físico que realizan las personas después 
# de la cuarentena y como les afecto durante la pandemia, debido a que muchas 
# personas dejaron de hacer ejercicio por el poco espacio que disponían en sus 
# hogares o simplemente por no disponer del equipo adecuado para realizar la 
# actividad física. Para este estudio, se monitoreó una muestra de 15 
# personas las cuales realizaron distintas rutinas de ejercicio, con el fin de 
# averiguar cual ha sido el impacto de la cuarentena en la vida de las personas 
# que realizan actividad física.

# Variables involucradas:
# Rutina de ejercicios: Variables categóricas y dicotómicas:
#   Cardio, Fuerza y Resistencia (valor 0 para no realizado y 1 para realizado)

# Para que se pueda realizar la prueba Q de Cochran se debe cumplir ciertas condiciones
# Para el caso expuesto se cumple que:
# Las variables de respuesta son dicotómicas.
# La variable independiente es categórica.
# Las observaciones son independientes entre sí.
# El tamaño de la muestra es lo suficientemente grande.

# Hipótesis a contrastar:
# H0: La proporción de personas que pueden lograr las rutinas es la misma para 
# todos los grupos.
# HA: La proporción de personas que pueden lograr las rutinas es distinta para 
# al menos un grupo.

# Tabla que ejemplifica si se puede o no realizar una rutina específica
# 0 si no puede realizarla, 1 si puede realizarla.
# ----------------------------------------------------------
#               Rutina de Ejercicios
# Personas  Cardio          Fuerza            Resistencia
# 1         1               1                 0
# 2         1               0                 1
# 3         1               1                 1
# 4         1               0                 0
# 5         0               1                 0
# 6         0               0                 0
# 7         0               1                 0
# 8         1               0                 1
# 9         1               1                 1
# 10        1               0                 0
# 11        0               1                 0
# 12        0               0                 1
# 13        1               1                 0
# 14        0               1                 1
# 15        0               0                 0

#-------------------------------------------------------------------------------