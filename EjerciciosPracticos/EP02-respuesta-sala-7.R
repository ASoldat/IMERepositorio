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

# Importar desde un archivo de valores separados por coma en formato ingl�s
# Se agrega la parte del encoding.
# Se debe elegir el archivo a leer, en este caso "EP-02 Datos Casen 2017.csv"
datos <- read.csv(file.choose(), encoding = 'UTF-8')

# Se filtra el dataframe con los datos relevantes al ejercicio los cuales son:
# sexo: sexo de la persona registrada
# comuna: comuna de la persona registrada
# ing.comuna: posici�n en el ranking hist�rico del ingreso de la comuna (ascendente)
# ytot: ingreso total

mujeres <- datos %>% filter ( sexo == "Mujer")
mujeres <- data.frame(mujeres$comuna,
                     mujeres$ing.comuna,
                     mujeres$ytot)

# Se obtiene la media de los ingresos totales, agrupando estos por cada comuna (se utiliza su ranking)

dataComunal <-  mujeres  %>% 
  group_by(mujeres.ing.comuna) %>% 
  summarise(promedio = mean(mujeres.ytot))


# Crear gr�fico de dispersi�n, que permita analizar con respecto a la pregunta.
# Discusion:
# Se decide utilizar la media del ingreso total obtenido de todas las mujeres por cada comuna,
# con el fin de poder visualizar con mayor facilidad la influencia de este promedio con el ranking de la comuna en la que habita.
# Adem�s, se utiliza un gr�fico de dispersi�n o de puntos, el cual permite ver si existe alguna relaci�n
# entre dos variables num�ricas, en este caso; el ranking de la comuna y el promedio de los ingresos.
# los cuales pueden tener una asociaci�n idependiente, positiva, o bien, negativa segun corresponda.

g <- ggscatter (dataComunal,
                x = "promedio",
                y = "mujeres.ing.comuna",
                color = "red",
                title = "Ranking de la Comuna v/s Promedio de sueldo de las mujeres en el municipio",
                xlab = "Promedio sueldo mujeres [Pesos]",
                ylab = "Ranking de la comuna")

print(g)

# Pregunta Sala 7:
# �Tiene relaci�n el ingreso de las mujeres de la RM con la riqueza del municipio donde habita?

# Respuesta:
# No, no tiene relaci�n el ingreso de las mujeres de la RM con la riqueza de su municipio.
# Se puede apreciar del gr�fico que existe una asociaci�n independiente con respecto a las dos variables.
# No se aprecia una correlaci�n positiva o negativa, por lo que se concluye que no tienen relaci�n.
# Finalmente, existen datos atipicos en la zona superior derecha del gr�fico, que afectan a la hora de concluir.

