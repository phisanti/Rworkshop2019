#####################################################################################
## Guion para explicar los comandos básicos de representación y exploración de datos
## a traves de la librería ggplot2
#####################################################################################

# Importar lib
library(ggplot2)
library(ggpubr)
library(data.table)

# Establecer una plantilla gráfica
theme_set(ggpubr::theme_pubclean(base_size = 15)) # Load theme

# Cuarteto de Anscombe, gráfica para mostar

anscombe 
anscombe_m <- data.frame() # Crear data.frame vacio para recoger 

for(i in 1:4) { # Transformar cuarteto de anscombe de formato ancho a formato largo
  anscombe_m <- rbind(anscombe_m, data.frame(set = i, x = anscombe[,i], y = anscombe[,i+4]))
}
rm(i)
anscombe_m

ggplot(anscombe_m, aes(x, y)) + 
  geom_point(size=2, color="red", fill="orange", shape=21) + 
  geom_smooth(method="lm", fill=NA, fullrange=TRUE) + 
  facet_wrap(~paste0("Set ", set), ncol=2)

# Como construir una gráfica --------------------------------------

# Crear una tabla de datos para trabajar

Sigma <- matrix(c(10,4,3,2),2,2) # Matriz de correlaciones para la función mvrnorm
Sigma

d <- data.table(lugar = sample(c("España", "Mexico"), 400, replace = TRUE)) # variable1 = 2 paises
d[, grupo := rep(c("a", "b"), each = 200)]                                  # variable2 = 2 grupos

# variable2 y 3: multivariate dist
set.seed(1) # Plantar semilla aleatoria

d[grupo == "a", 
  c("x", "y") :=  
    data.table(MASS::mvrnorm(n = 200, rep(1, 2), Sigma) *1.75)]             
d[grupo == "b", 
  c("x", "y") :=  
    data.table(MASS::mvrnorm(n = 200, rep(1, 2), Sigma) + 1.5)]
d[grupo == "b", y := y * 1.5 ]
d # Mostrar el resultado

# Sección 1: Gráficas univariantes ------------------------------------------------

# Histograma
ggplot(d, aes(x = y)) +
  geom_histogram(fill = "cornflowerblue", col = "gray80")   # Argumento bins por defecto

ggplot(d, aes(x = y)) +
  geom_histogram(fill = "cornflowerblue", col = "gray80", 
                 bins = 64)                                 # Argumento bins declarado

# Gráfico de cuantiles
ggplot(d, aes(sample = y)) +
  geom_qq(col = "cornflowerblue") +
  geom_qq_line(distribution = qnorm)

# Sección 2: Gráficas bivariantes categóricas --------------------------------------

# Diagrama de barras
ggplot(d, aes(x = grupo, y = y)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") 

# Diagrama de cajas y bigotes
ggplot(d, aes(x = grupo, y = y)) +
  geom_boxplot(fill = "cornflowerblue") 

# Sección 4: Gráficos bivaiantes continuos -----------------------------------------

ggplot(d, aes(x = x, y = y, shape = grupo)) +
  geom_point(col = "black", # color fijo 
             size = 2) +
  theme_bw(base_size = 20)

ggplot(d, aes(x = x, y = y, col = grupo)) + # color como variable
  geom_point(size = 2) +
  theme_bw(base_size = 20)

# Sección 5: Representación de modelos simples ---------------------------------------

ggplot(d, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth() 

ggplot(d, aes(x = x, y = y)) +
  geom_point(aes(col = grupo)) +
  stat_smooth(method = "lm", col = "black")

# Sección 6: Representación de modelos con subsetado y distitos datos -------------------

ggplot(d, # Datos originales 
       aes(x = x, y = y, col = grupo)) +
  geom_point() +                                  # Representar como puntos
  stat_smooth(method = "lm",                      # Regresión lineal
              formula = y ~ exp(x)) +             # Especificar fórmula
  
  # Introducir seguntods datos, 
  geom_line(data = d[, .(meanY = mean(y)),
              by = .(x = round(x), grupo, lugar)],
            
            aes(x = x, y = meanY),                # Representar como lineas discontinueas
            linetype = 2, size = 1.5) +
 
  facet_grid(~ lugar,                             # Separar datos por lugar
             scales = "free_x") +
  scale_x_continuous(trans = "log2") +
  labs(x = "Una variable", y = "Una respuesta") +
  scale_color_brewer(palette = "Set1")

