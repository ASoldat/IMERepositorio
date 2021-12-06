# EP-05 Sala 7
# Arturo Cadenas (20.468.370-0)
# Stephan Silva (20.298.778-8)
# Ivan Zuñiga (20.003.345-0)

if (!require(Hmisc)){
  install.packages("Hmisc", dependencies = TRUE )
  require (Hmisc)
}

# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) 
# (Journal of chronic diseases, 25(12), 711-716) sobre la incidencia de la 
# cantidad de alcohol y de tabaco que se consume en el riesgo de padecer cáncer 
# oral. Las tablas muestran el número de personas que consumiendo una cierta
# cantidad de alcohol o de tabaco, mostrada en cada fila, desarrollaron o no 
# desarrollaron (controles) la enfermedad durante su vida.


# Pregunta 1

# Estudios previos habían determinado que la incidencia de cáncer oral en la 
# población general que no fuma era de 18%. ¿Respaldan estos datos tal estimación? 


#H0: La incidencia de cáncer oral en la población general que no fuma es 
# de 18% (p = 0,18)

#HA: La incidencia de cáncer oral en la población general que no fuma es 
# distinta de 18% (p != 0,18)

# Tamaño muestra
n <- 111
# Probabilidad de éxito
p_exito <- 26/n
# Nivel de significación.
alfa <- 0.05
# Valor nulo
valor_nulo <- 0.18
# Calcular cantidad de éxitos y fracaso .
exitos <- p_exito * n
fracaso <- (1-p_exito) * n

# Para poder realizar la prueba de Wilson se deben verificar dos condiciones.
# - Dado el estudio realizado se puede concluir que las observaciones de la muestra
# son independientes entre si, pues los resultados estas no influyen en los demás

# - Se cumple la condición de éxito-fracaso, pues existen más de 10 observaciones
# correspondientes a éxitos (26 casos) y más de 10 observaciones correspondientes 
# a fracasos (85 casos)


# Por lo que se procede con la prueba de Wilson en R.
prueba1 <- prop.test(exitos , n = n , 
                    p = valor_nulo ,
                    alternative = "two.sided", 
                    conf.level = 1 - alfa)

print(prueba1)

# Resultado:
# Con respecto a la prueba realizada y al valor p obtenido de 0.1726, este es
# mayor al valor de alfa de 0.05 (respecto al nivel de significación), por lo 
# que no hay evidencia suficiente para rechazar la hipótesis nula (se falla al rechazar)
# de esta manera no se puede concluir correctamente y, los datos respaldan la
# estimación de que la incidencia de cáncer oral en la población general que no fuma es de 18%.
#-------------------------------------------------------------------------------

# Pregunta 2


# Según estos datos, ¿da lo mismo no fumar que hacerlo diariamente consumiendo 
# entre 1 y 19 cigarrillos? 

#H0: La diferencia de la proporción de personas que desarrollan la enfermedad en 
# la población que no fuma y la que fuma entre 1 y 19 cigarrillos es cero (p1 - p2 = 0)

#HA: La diferencia de la proporción de personas que desarrollan la enfermedad en 
# la población que no fuma y la que fuma entre 1 y 19 cigarrillos es distinta de
# cero (p1 - p2 != 0)

# Fijar valores conocidos ( no fuma , fuma entre 1-19 cigarrillos )
n2 <- c(c(111 , 163))

exitos2 <- c(26 , 66)
fracasos2 <- c(85, 97)

# Al igual que antes, para poder realizar la prueba de Wilson se deben verificar dos condiciones.
# - En el estudio realizado se puede concluir que las observaciones de la muestra
# son independientes entre si, pues los resultados estas no influyen en los demás

# - Se cumple la condición de éxito-fracaso, pues existen más de 10 observaciones
# correspondientes a éxitos (26 para aquellos que no fuman y 66 para aquellos que
# fuman entre 1 y 19 cigarrillos) y más de 10 observaciones correspondientes a 
# fracasos (85 para aquellos que no fuman y 97 para aquellos que fuman entre 1 y
# 19 cigarrillos)

# Por lo que se procede con la prueba de Wilson en R.
prueba2 <- prop.test(exitos2 , n = n2 , 
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

print(prueba2)

# Resultado:
# Con respecto a la prueba realizada y al valor p obtenido de 0.00501, este es
# menor al valor de alfa de 0.05 (respecto al nivel de significación), por lo
# que se rechaza la hipótesis nula a favor de la hipótesis alternativa, de esta
# manera se concluye con 95% de confianza que la diferencia de proporción entre 
# las dos poblaciones es distinta de cero, es decir, NO da lo mismo no fumar 
# que hacerlo diariamente consumiendo entre 1 y 19 cigarrillos.
#-------------------------------------------------------------------------------

# Pregunta 3

# Suponiendo que la diferencia en la proporción de personas que desarrollan la 
# enfermedad entre quienes no fuman y aquellos que fuman de 1 a 19 cigarrillos 
# al día es de 0.25. ¿Cuánta gente deberíamos monitorear para obtener un 
# intervalo de confianza del 95% y poder estadístico de 90%? si se intente
# mantener aproximadamente la misma proporción de gente estudiada en cada caso.

# Poder estadístico
poder <- 0.9
# Nivel de significación
alfa <- 0.05
# Probabilidad de éxitos
prob1 <- 26/111
prob2 <- 66/163
# diferencia de proporción
proporcion <- 0.25

# Función para obtener el número de observaciones
# Se opta en utilizar la función bsamsize debido a que se deben utilizar dos 
# muestras independientes, y el cual calcula los tamaños de cada grupo
prueba3 <- bsamsize(p1 = prob1,
                    p2 = prob2,
                    fraction = proporcion,
                    alpha = alfa,
                    power = poder)

print(prueba3)

# Resultado:
# Con respecto a la prueba realizada y con los datos entregados, se debería
# monitorear a una cantidad de 105 personas entre quienes no fuman, y una 
# cantidad de 313 personas entre aquellos que fuman de 1 a 19 cigarrillos para 
# obtener un intervalo de confianza del 95% y poder estadístico de 90%, 
# manteniendo una diferencia de proporción de personas que desarrollan la 
# enfermedad de 0.25.
#-------------------------------------------------------------------------------


