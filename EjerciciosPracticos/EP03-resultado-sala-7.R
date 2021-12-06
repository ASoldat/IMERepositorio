# Script Sala 7

# Uso de packages necesarios
#library (dplyr)
#library (ggpubr)

if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

# Lectura archivo de entrada 
# Importar desde un archivo de valores separados por coma en formato inglés
# Se agrega la parte del encoding.
# Se debe elegir el archivo a leer, en este caso "EP-03 Datos Casen 2017.csv"
población <- read.csv(file.choose(), encoding = 'UTF-8')
# Obtención de datos importantes (script de ejemplo)
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

# ||||||||||PARTE 1||||||||||

# Se define la semilla y se obtienen 5000 datos
set.seed(1412)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# Gráfico Distribución Normal
plot(density(ingreso.normal), main = "Distribución Normal", 
     xlab="EjeX",
     ylab="EjeY")

# Generación de la correspondiente distribución Z
Z <- (ingreso.normal - media.ingreso)/sd.ingreso

# Gráfico Distribución Z
plot(density(Z), main = "Distribución Z", 
     xlab="EjeX",
     ylab="EjeY")

# Con la distribución Z obtenida se crean:
# dos distribuciones chi cuadrado, cada una con más de 3 y menos de 15 grados de libertad


# Calcular distribución chi-Cuadrado 

# Datos iniciales y auxiliares
chiCuadrado5G <- Z
chiCuadrado6G <- Z
# Iterar cantidad de datos
for(i in 1:5000)
{
  # Iterar por grados de libertad (5 grados)
  for(k in 1:5)
  {
    if(k 	!= 1)
    {
      chiCuadrado5G[i] <- chiCuadrado5G[i] + chiCuadrado5G[i]^2
    }
    else
    {
      chiCuadrado5G[i] <- chiCuadrado5G[i]^2
    }
  }
}

# Iterar cantidad de datos
for(i in 1:5000)
{
  # Iterar por grados de libertad (6 grados)
  for(k in 1:6)
  {
    if(k 	!= 1)
    {
      chiCuadrado6G[i] <- chiCuadrado6G[i] + chiCuadrado6G[i]^2
    }
    else
    {
      chiCuadrado6G[i] <- chiCuadrado6G[i]^2
    }
  }
}

# Gráfico distribución chi cuadrado de  5 grados
plot(density(chiCuadrado5G), main = "chiCuadrado 5 grados", 
     xlab="EjeX",
     ylab="EjeY",
     xlim=c(0,3*10^17))

# Gráfico distribución chi cuadrado de 6 grados
plot(density(chiCuadrado6G), main = "chiCuadrado 6 grados", 
     xlab="EjeX",
     ylab="EjeY",
     xlim=c(0,5*10^36))

# Usando las dos chi cuadrados generadas, se construye una distribución F.

distriF <- (chiCuadrado5G/5) / (chiCuadrado6G/6)

# Gráfico distribución F
plot(density(distriF), main = "Distribución F", 
     xlab="EjeX",
     ylab="EjeY")


# ||||||||||PARTE 2||||||||||

# Obtención de 25 repeticiones de un ensayo de Bernoulli con éxito

set.seed(1412)
n.repeticiones <- 25
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
num.repeticiones <- sapply(1:n.repeticiones, ensayo)

# Se obtiene la probabilidad de éxito
probExito <- sum(num.repeticiones)/length(num.repeticiones)


 