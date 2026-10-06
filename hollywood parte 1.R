# CASO 2 : HOLLYWOOD 
# ANALÍTICA DE DATOS
# Juliana Perez, Veronica Sandoval y Camila Salamanca

names(Hollywood) <- trimws(names(Hollywood))
names(Hollywood)
head(Hollywood)

# Revisamos cuántas películas y variables tiene la base.
dim(Hollywood)

# PUNTO 1
# DESCRIPCIÓN INICIAL DE LOS DATOS

# PUNTO 1.1 - Calculamos el ingreso mínimo, promedio y máximo obtenido
# durante el primer fin de semana de estreno.
min(Hollywood$`Opening Gross`)
mean(Hollywood$`Opening Gross`)
max(Hollywood$`Opening Gross`)

# PUNTO 1.2 - Calculamos el ingreso mínimo, promedio y máximo obtenido
# por las películas en Estados Unidos.

min(Hollywood$`Total U.S. Gross`)
mean(Hollywood$`Total U.S. Gross`)
max(Hollywood$`Total U.S. Gross`)

# PUNTO 1.3 - Calculamos el ingreso mínimo, promedio y máximo obtenido
# por las películas fuera de Estados Unidos.

min(Hollywood$`Total Non-U.S. Gross`)
mean(Hollywood$`Total Non-U.S. Gross`)
max(Hollywood$`Total Non-U.S. Gross`)

# PUNTO 1.4 - Calculamos el número mínimo, promedio y máximo de salas
# en las que se estrenaron las películas.

min(Hollywood$`Opening Theatres`)
mean(Hollywood$`Opening Theatres`)
max(Hollywood$`Opening Theatres`)

# PUNTO 1.5 - Contamos cuántas películas pertenecen al género Comedy.

sum(Hollywood$Genre == "Comedy")

# PUNTO 1.6 - Contamos cuántas películas tienen clasificación R.

sum(Hollywood$MPAA == "R")

# PUNTO 2
# A - CALCULAR EL ROI

Hollywood$ROI_US <-
  (Hollywood$`Total U.S. Gross` - Hollywood$Budget) /
  Hollywood$Budget

head(Hollywood$ROI_US)

# Calculamos el ROI promedio de todas las películas.

mean(Hollywood$ROI_US)

# Convertimos el ROI promedio a porcentaje

mean(Hollywood$ROI_US) * 100

# PUNTO 2B - Calculamos un intervalo de confianza del 95% para estimar
# el rango en el que podría encontrarse el ROI promedio.

t.test(Hollywood$ROI_US,
       conf.level = 0.95)

# PUNTO 2C - Queremos comprobar si el ROI promedio de las películas
# es significativamente mayor al 12% mencionado en el caso.
#
# H0: El ROI promedio es menor o igual al 12%.
# H1: El ROI promedio es mayor al 12%.
#
# Si el p-value es menor a 0.05, rechazamos H0.

t.test(Hollywood$ROI_US,
       mu = 0.12,
       alternative = "greater")

# PUNTO 3A - Queremos comparar los ingresos en Estados Unidos
# de las comedias frente a las películas de otros géneros.

ingresos_comedias <-
  Hollywood$`Total U.S. Gross`[Hollywood$Genre == "Comedy"]

# Guardamos los ingresos de las películas de otros géneros.

ingresos_otros <-
  Hollywood$`Total U.S. Gross`[Hollywood$Genre != "Comedy"]

# Calculamos el ingreso promedio de las comedias.

mean(ingresos_comedias)

# Calculamos el ingreso promedio de los otros géneros.

mean(ingresos_otros)

# Realizamos una prueba t para determinar si existe
# una diferencia estadísticamente significativa entre
# los ingresos de las comedias y los otros géneros.
#
# H0: No existe diferencia en los ingresos promedio.
# H1: Sí existe diferencia en los ingresos promedio.
#
# Si el p-value es menor a 0.05, rechazamos H0.

t.test(ingresos_comedias,
       ingresos_otros)

# GRÁFICA DEL PUNTO 3A - Creamos una variable para clasificar las películas
# entre "Comedia" y "Otros géneros".

Hollywood$Tipo_Genero <-
  ifelse(Hollywood$Genre == "Comedy",
         "Comedia",
         "Otros géneros")

# Cargamos ggplot2 para realizar la gráfica.

library(ggplot2)

# Creamos un boxplot para comparar los ingresos.

ggplot(Hollywood,
       aes(x = Tipo_Genero,
           y = `Total U.S. Gross` / 1000000)) +
  
  geom_boxplot() +
  
  labs(
    title = "Ingresos en Estados Unidos según género",
    subtitle = "Comparación entre comedias y otros géneros",
    x = "Tipo de película",
    y = "Ingresos totales en EE. UU. (millones de USD)"
  ) +
  
  theme_minimal()

# PUNTO 3B - COMPARACIÓN DEL ROI
# Guardamos el ROI de las películas de comedia.

ROI_comedias <-
  Hollywood$ROI_US[Hollywood$Genre == "Comedy"]

# Guardamos el ROI de las películas de otros géneros.

ROI_otros <-
  Hollywood$ROI_US[Hollywood$Genre != "Comedy"]

# Calculamos el ROI promedio de las comedias.

mean(ROI_comedias)

# Convertimos el ROI promedio de las comedias a porcentaje.

mean(ROI_comedias) * 100

# Calculamos el ROI promedio de los otros géneros.

mean(ROI_otros)

# Convertimos el ROI promedio de los otros géneros a porcentaje.

mean(ROI_otros) * 100

# Realizamos una prueba t para determinar si existe
# una diferencia estadísticamente significativa entre
# el ROI de las comedias y el ROI de los otros géneros.
#
# H0: No existe diferencia en el ROI promedio.
# H1: Sí existe diferencia en el ROI promedio.
#
# Si el p-value es menor a 0.05, rechazamos H0.

t.test(ROI_comedias,
       ROI_otros)

# GRÁFICA DEL PUNTO 3B
# Creamos un boxplot para comparar visualmente el ROI
# de las comedias frente a los otros géneros.

ggplot(Hollywood,
       aes(x = Tipo_Genero,
           y = ROI_US * 100)) +
  
  geom_boxplot() +
  
  labs(
    title = "ROI de las películas según género",
    subtitle = "Comparación entre comedias y otros géneros",
    x = "Tipo de película",
    y = "ROI en Estados Unidos (%)"
  ) +
  
  theme_minimal()
