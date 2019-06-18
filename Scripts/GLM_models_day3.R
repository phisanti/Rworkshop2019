#####################################################################################
## Guion para explicar los comandos para ejecutar regresiones GLM
## Explorar los datos tipo glm y checar modelos
#####################################################################################

library(glmmTMB)
library(data.table)
library(tidyverse)
library(gridExtra)

theme_set(ggpubr::theme_pubclean(base_size = 15)) # Load theme

# Cargar datos

d <- fread("Datasets/BrownFat_2011.csv")

# Sección 1: Gráficos de checkeo ------------------------------------------------------------
 
d %>% 
  ggplot(., aes(x = Total_vol, y = ..density..)) +
  geom_histogram(bins = 64, col = "grey80", fill = "cornflowerblue") +
  labs(x = "Volumen", y = "Frecuencia", title = "Distribución Grasas pardas en la población")

d[Total_vol > 0] %>% 
  ggplot(., aes(x = Total_vol))  +
  geom_histogram(bins = 64, col = "grey80", fill = "cornflowerblue")+
  labs(x = "Volumen", y = "Frecuencia", title = "Distribución Grasas pardas en grupo positivo")

d %>% 
  ggplot(., aes(sample = Total_vol)) +
  geom_qq(col = "cornflowerblue") +
  geom_qq_line(distribution = qnorm) 

m6.1 <- lm(Total_vol ~ BMI + Cancer_Status  + Sex + Season, 
           data = d) 
par(mfrow = c(2,2), 
    mar = c(4.5, 4.5, 2.5, 2.5))
plot(m6.1, cex.axis = 1.2, cex.lab = 1.5, cex.main = 12, cex.sub = 20)
par(mfrow = c(1,1)) 
    
hist(residuals(m6.1), main = "Residuals")

m6.1.1 <- glmmTMB(Total_vol ~ BMI + Cancer_Status  + Sex + Season, 
                  data = d, family = gaussian) 


m6.2 <- glmmTMB(Total_vol ~ BMI + Cancer_Status  + Sex + Season, 
                data = d, 
                family = tweedie(link = "log"))

AIC(m6.1, m6.1.1, m6.2)

summary(m6.2)

# Presentar resultados GLM

plot_model(model = m6.2, sort.est = T, show.values = TRUE, transform = NULL)
plot_model(model = m6.2, sort.est = T, show.values = TRUE, transform = "exp")

# Verificar el modelo

par(mfrow = c(1,3), 
    mar = c(5, 5, 2.5, 2.5),
    cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.2, cex.sub = 20)

plot(fitted(m6.2), residuals(m6.2), 
     xlab = "Valor Ajustado", ylab = "Valor Residual")
qqnorm(resid(m6.2))
qqline(resid(m6.2))
hist(resid(m6.2), xlab = "Residuos", main = "")

# Sección 2: Modelar la dispersion y los ceros ----------------------------------------

d[, .(BrownFat, Age, Diabetes, Cancer_Status)] %>%
  melt.data.table(., id.vars = "BrownFat", verbose = F) %>%
  .[, .(Count = .N), by = .(variable, value, BrownFat)] %>%
  
  ggplot(., aes(x = as.factor(value),y = Count, fill = as.factor(BrownFat)) ) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ variable, scales = "free_x", nrow = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "", y = "Población con grasa parda") +
  ggpubr::theme_pubclean(base_size = 20)

# Predecir los 0
m7.1 <- glmmTMB(Total_vol ~ BMI + Cancer_Status  + Sex + Season, 
                ziformula =  ~ Diabetes + Age,
                data = d, 
                family = tweedie(link = "log"))
summary(m7.1)

# Modelar la dispersión

d[Total_vol > 0] %>%
  ggplot(., aes(x = Total_vol)) +
  geom_histogram(fill = "cornflowerblue", col = "gray80", bins = 64) +
  facet_grid(Sex ~ .) +
  labs(x = "Volumen de Grasa Parda", y = "Conteo")

# Predecir dispersión
m7.2 <- glmmTMB(Total_vol ~ BMI + Cancer_Status + Season + Sex, 
                dispformula = ~ Sex,
                data = d, 
                family = tweedie(link = "log"))

# Predecir 0 y dispersion
m7.3 <- glmmTMB(Total_vol ~ BMI + Cancer_Status + Season + Sex, 
                dispformula = ~ Sex,
                ziformula =  ~ Diabetes + Age,
                                data = d, 
                family = tweedie(link = "log"))


AIC(m6.2, m7.1, m7.2, m7.3) %$% .[order(AIC),]

par(mfrow = c(1,3), 
    mar = c(5, 5, 2.5, 2.5),
    cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.2, cex.sub = 20)

plot(fitted(m7.2), residuals(m7.3), 
     xlab = "Valor Ajustado", ylab = "Valor Residual")
qqnorm(resid(m7.3))
qqline(resid(m7.3))
hist(resid(m7.3), breaks = 64, xlab = "Residuos", main = "")

p  <- plot_model(m7.3,type = "pred",  
                 vcov.fun = "vcovCL") 
do.call("grid.arrange", c(p, ncol = 4))

