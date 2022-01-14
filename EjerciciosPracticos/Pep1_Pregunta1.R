# PEP1
# Arturo Cadenas
# 20.468.370-0

# Importación de datos
# Se le debe ingresar el archivo "Datos PEP 1.csv"
datos <- read.csv2(file.choose (), encoding = 'UTF-8')

# Datos Conocidos
n <- 40
valor_nulo <- 2.5
alfa <- 0.05

# Hipótesis:
# H0: el promedio de las diferencias del índice de masa muscular es igual a
#   2,5 [Kg/m^2] tras el periodo de entrenamiento. ( udif = 2,5)

# HA: el promedio de las diferencias del índice de masa muscular es distinto a
#   2,5 [Kg/m^2] tras el periodo de entrenamiento.  (udif != 2,5)

# Se calculan las diferencias
diferencias <- datos$imc_1 - datos$imc_2

# Se obtiene una muestra aleatoria con los datos pedidos.
# Se define la semilla y se obtienen 40 datos
set.seed(523)
datos.muestra <- rnorm(n, mean = mean(diferencias), sd = sd(diferencias))

# Validaciones para utilizar prueba t de student

# La muestra fue escogida de forma al azar, por lo que se puede asumir que las
# observaciones son independientes, además la muestra de 40 reclutas no supera 
# al 10% de la población.
# Luego se debe tener que la muestra se acerque razonablemente a una distribución
# normal, por lo que se utiliza la prueba de Shapiro-wilk

# Verificar si la distribución se acerca a la normal .
normalidad <- shapiro.test(datos.muestra)
print(normalidad)

# Hipótesis prueba shapiro Wilk
# H0: La muestra fue extraída desde una distribución normal.
# HA: La muestra fue extraída desde una distribución diferente a la normal.

# En conclusion se falla al rechazar la hipótesis nula, con un p = 0.3237
# mayor al nivel de significancia, por lo que se puede
# suponer normalidad en la muestra de los datos obtenidos.

# Aplicar la prueba t de Student a la diferencia de medias .
prueba_1 <- t.test(datos.muestra,
                   alternative = "two.sided",
                   mu = valor_nulo ,
                   conf.level = 1 - alfa )

print(prueba_1)

# Finalmente se concluye al realizar la prueba t, con p = 0.5031 mucho mayor al 
# nivel de significación por lo tanto se falla al rechazar la hipótesis nula.
# por lo que se afirma con un 95% de confianza que el promedio de las diferencias
# entre el índice de masa muscular antes y después del periodo de entrenamiento
# es igual a  2,5 [Kg/m^2].