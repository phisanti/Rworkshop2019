#####################################################################################
## Guion para explicar los comandos para ejecutar modelos GAM
## Verificar sobre ajuste y entender el modelo en si
#####################################################################################

library(mgcv)
library(data.table)
library(tidyverse)
library(ggpubr)

theme_set(ggpubr::theme_pubclean(base_size = 15)) # Load theme

# Qué es una transformación

MASS::Animals %>%
  ggplot(., aes(body, brain)) +
  geom_point(col = "cornflowerblue") +
  stat_smooth(method = "lm", linetype = 2, col = "gray50", se = F)

MASS::Animals %>%
  ggplot(., aes(log(body), log(brain))) +
  geom_point(col = "cornflowerblue") +
  stat_smooth(method = "lm", linetype = 2, col = "gray50", se = F)

# Sección 1: Relaciones no lineales --------------------------------------------------------

set.seed(1)
Exm <- data.table(x = runif(100, min = -3, max = 12))
Exm[, y := sin(x/2) + rnorm(100, 0, 0.5) + 2]

ggplot(Exm, aes(x, y)) +
  geom_point(size = 2, col = "cornflowerblue") +
  stat_smooth(method = "lm", se = F, col = "black")

ggplot(Exm, aes(x, y)) +
  geom_point(size = 2, col = "cornflowerblue") +
  stat_smooth(method = "lm", se = F, col = "grey60") +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 6, bs = "cc"),
              se = F, col = "black", size = 1.25)

# Construir modelo aditivo
mtest <- gam(y ~ s(x, k = 6, bs = "cc"), data = Exm)

# Extraer funciones base
m_matrix <- predict(mtest, type = "lpmatrix")
m_matrix <- m_matrix %*% diag(coef(mtest) + 1.737) # Escalar con el coneficiente y el intercepto

# Reformatear funciones base
m_matrix <- data.table(m_matrix) 
setnames(m_matrix, names(coef(mtest)))
m_matrix[, x := Exm$x]
m_matrix <- m_matrix %>%
  melt.data.table(., id.vars = "x", value.name = "y")

# Representar
ggplot(m_matrix[!variable == "(Intercept)"]) +
  geom_point(data = Exm, aes(x = x, y = y), size = 2, col = "cornflowerblue") +
  geom_line( aes(x = x, y = y, col = variable), size = 1.2, linetype = 2) +
  stat_smooth(data = Exm, aes(x = x, y = y),
              method = "gam", formula = y ~ s(x, k = 6, bs = "cc"),
              se = F, col = "black", size = 1.25) +
  scale_color_brewer(palette = "Set2", name = "Función base:")

# Sección 2: Ejemplo de modelo aditivo ----------------------------------------------

d <- fread("Datasets/BrownFat_2011.csv")
d[, Cancer_Status := as.factor(Cancer_Status)]
d[, Sex := as.factor(Sex)]

ggplot(d, aes(x = Weight , y = LBW)) +
  geom_point(alpha = 0.3) +
  scale_color_brewer(palette = "Set1", name = "Género:") +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3, bs = "cc")) +
  labs(x = "Peso - Kg", y = "Índice LBW")

m0 <- lm(LBW ~ Weight, data = d)

m1 <- gam(LBW ~ s(Weight , k = 3), data = d, 
          method = "REML")

# Verificar el modelo

gam.check(m1)

# Ajustar numero de nodos

m1.2 <- gam(LBW ~ s(Weight , k = 9), data = d, 
            method = "REML")
gam.check(m1.2)

# Representación final

ggplot(d, aes(x = Weight , y = LBW, col = Sex)) +
  geom_point(alpha = 0.3) +
  scale_color_brewer(palette = "Set1", name = "Género:") +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 9, bs = "cc")) +
  labs(x = "Peso - Kg", y = "Índice LBW")

m1.3 <- gam(LBW ~ s(Weight , k = 9) + Sex, data = d, 
            method = "REML")

# Modelo para mostrar el efecto shrinkage o hundimiento

m1.4 <- gam(LBW ~ s(Weight , bs = "ts") + s(Day, bs = "ts"), 
            data = d, method = "REML")
gam.check(m1.4)

# Sección 3: Construcción de modelos mas complejos ------------------------------------

m2 <- gam(LBW ~ s(Weight , bs = "fs",by = Sex) , data = d, 
          method = "REML")

m3 <- gam(LBW ~ s(Weight ) + Height , data = d, 
          method = "REML")

m4 <- gam(LBW ~ s(Weight ) + s(Height) , data = d, 
          method = "REML")

m5 <- gam(LBW ~ s(Weight , Height) + s(Weight ) + s(Height), 
          data = d, method = "REML")

AIC(m1, m2, m3, m4, m5) %>% data.table(., keep.rownames = T) %>% .[order(AIC)]

m6 <- gam(LBW ~ te(Weight , Height) + s(Weight ) + s(Height), 
          data = d, method = "REML")
m7 <- gam(LBW ~ te(Weight , Height, by = Sex) + s(Weight ) + s(Height), 
          data = d, method = "REML")

AIC(m5, m6, m7) %>% data.table(., keep.rownames = T) %>% .[order(AIC)]

# Sección 4: Representación visual del modelo ---------------------------------

par(mfrow=c(1, 3))


# Mostrar los efectos de cada variable
plot(m7,  residuals = T,  all.terms = T, 
     shade = TRUE, shade.col = "gray80",  cex = 0.5, pch = 16,
     cex.main = 1.25, cex.lab = 1.5, cex.axis = 1.5)

# Comparar predicción con valor real

plot(fitted(m7), model.frame(m7)[,1], 
     xlab = "Predicción", ylab = "Real", main = "Respuesta vs Ajuste")
abline(a = 0, b = 1, col = "red", lty = 2)

hist(resid(m7), breaks = 100, 
     border = "gray70", col="cornflowerblue",
     main = "Histograma de errores", xlab = bquote("y"[i]~" - "~hat(y)))

qq.gam(m7, s.rep = 10, rep = 10, cex = 0.5, pch = 16)
par(mfrow=c(1, 1),
    mar = c(5, 5, 5, 5))

# Representar interacci'on entre 2 variables v1 = gráfico de contorno 

vis.gam(m6, too.far = 1,
        plot.type = "contour", main = "Efecto peso-altura sobre LBW",
        cex.main = 1.8, cex.lab = 1.5, cex.axis = 2)

par(mfrow=c(2, 2),
    mar = c(1.8, 1.8, 1.8, 1.8))

for(An in c(0, 45, 90, 135)) {
  vis.gam(m6 , 
          theta = An, color = "topo", plot.type = "persp",
          zlab = "LBW", xlab = "Altura - cm", ylab = "Peso - Kg",
          cex.main = 1.8, cex.lab = 1.5, cex.axis = 2)
  rm(An)
}



