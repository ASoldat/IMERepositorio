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
# Importar desde un archivo de valores separados por coma en formato ingl�s
# Se agrega la parte del encoding.
# Se debe elegir el archivo a leer, en este caso "EP-03 Datos Casen 2017.csv"
poblaci�n <- read.csv(file.choose(), encoding = 'UTF-8')
# Obtenci�n de datos importantes (script de ejemplo)
tama�o <- nrow(poblaci�n)
ingreso <- as.numeric(poblaci�n[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tama�o.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tama�o.podado )

# ||||||||||PARTE 1||||||||||

# Se define la semilla y se obtienen 5000 datos
set.seed(1412)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# Gr�fico Distribuci�n Normal
plot(density(ingreso.normal), main = "Distribuci�n Normal", 
     xlab="EjeX",
     ylab="EjeY")

# Generaci�n de la correspondiente distribuci�n Z
Z <- (ingreso.normal - media.ingreso)/sd.ingreso

# Gr�fico Distribuci�n Z
plot(density(Z), main = "Distribuci�n Z", 
     xlab="EjeX",
     ylab="EjeY")

# Con la distribuci�n Z obtenida se crean:
# dos distribuciones chi cuadrado, cada una con m�s de 3 y menos de 15 grados de libertad


# Calcular distribuci�n chi-Cuadrado 

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

# Gr�fico distribuci�n chi cuadrado de  5 grados
plot(density(chiCuadrado5G), main = "chiCuadrado 5 grados", 
     xlab="EjeX",
     ylab="EjeY",
     xlim=c(0,3*10^17))

# Gr�fico distribuci�n chi cuadrado de 6 grados
plot(density(chiCuadrado6G), main = "chiCuadrado 6 grados", 
     xlab="EjeX",
     ylab="EjeY",
     xlim=c(0,5*10^36))

# Usando las dos chi cuadrados generadas, se construye una distribuci�n F.

distriF <- (chiCuadrado5G/5) / (chiCuadrado6G/6)

# Gr�fico distribuci�n F
plot(density(distriF), main = "Distribuci�n F", 
     xlab="EjeX",
     ylab="EjeY")


# ||||||||||PARTE 2||||||||||

# Obtenci�n de 25 repeticiones de un ensayo de Bernoulli con �xito

set.seed(1412)
n.repeticiones <- 25
ensayo <- function(x)
  ifelse(sample(poblaci�n[["sexo"]], 1) == "Mujer", 1, 0)
num.repeticiones <- sapply(1:n.repeticiones, ensayo)

# Se obtiene la probabilidad de �xito
probExito <- sum(num.repeticiones)/length(num.repeticiones)


 