#.......


# __________________________________________________________FECHA: 17 SEPT 2020

# Simular una base inventada y de ahí calcular múltiples veces el x barra para
#   ver que en promedio le atina
nsim <- 1000 # Cantidad de simulaciones que voy a hacer
N <- 10000 # Tamaño de población
n <- 100 # Tamaño de muestra para cada simulación

#   Lo que está aquí son todas las observaciones.
#   En general, estos son desconocidos, pues son poblacionales
base.datos <- data.frame (x = rpois (N, lambda = 4)) # Esta es la U en la notación
                                                      # Matriz Universo
media.U <- mean (base.datos$x) # Este es el valor real que aproximo con las muestras

media.muestra <- rep (NA, nsim)

#   Ciclo a través de las 1000 simulaciones
for (i in 1:nsim){
  muestra <- sample (base.datos$x, n, replace = FALSE) # Muestra simple s/r
  media.muestra[i] <- mean(muestra) # cada uno de estos x barra
}

mean(media.muestra)

library (ggplot2)
ggplot()+
  geom_histogram (aes (x = media.muestra, y = ..density..), fill = "red")+
  geom_vline (aes (xintercept = media.U), linetype = "dashed")


# Ahora calculamos la varianza de la media
#   Var(xbarra_s) = (1-f)/n * s^2_U
varianza.xbar <- ((1-n/N)/n)*var(base.datos$x)

varianza.s.xbar <- rep (NA, nsim)
for (i in 1:nsim){
  muestra <- sample (base.datos$x, n, replace = FALSE)
  varianza.s.xbar[i] <- ((1-n/N)/n)*var(muestra)
}
mean(varianza.s.xbar)

ggplot()+
  geom_density (aes (x = varianza.s.xbar), color = "red")+
  geom_vline (aes (xintercept = varianza.xbar))







