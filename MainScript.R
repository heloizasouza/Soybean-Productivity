rm(list = ls())

# Libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(sp)
library(nlme)
library(lme4)
library(rpart)
library(rpart.plot)

# Data Treatment ----------------------------------------------------------


# loading the final transformed data
soybean_data <- read.csv(file = "data_join.csv")
attach(soybean_data)

soybean_data <- soybean_data |>
  mutate(Solo = as.factor(Solo), Cultivar = as.factor(Cultivar),
         Caracteristica = as.factor(Caracteristica), 
         Clima = factor(x=Clima, levels=c("ModerLaNina","FracoLaNina","Neutro")),
         Ciclo3 = factor(x=Ciclo3, levels=c("Precoce","Medio","Tardio")), 
         Ciclo4 = factor(x=Ciclo4, levels=c("Super Precoce","Precoce","Medio","Tardio")))

levels(soybean_data$Ciclo4)


# Descriptive Analysis ----------------------------------------------------

attach(soybean_data)

##### Densidade por tipo de SOLO
sample_size = soybean_data %>% group_by(Solo) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Solo, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Solo") +
  xlab("")


##### Densidade por CLIMA
sample_size = soybean_data %>% group_by(Clima) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Clima, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Clima)) +
  geom_violin(width=1.4) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Característica Climática") +
  xlab("")


##### Densidade por CICLO
sample_size = soybean_data %>% group_by(Ciclo4) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ciclo4, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Ciclo4)) +
  geom_violin(width=1.4) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Característica Climática") +
  xlab("")


##### Densidade por CICLO e CLIMA
sample_size = soybean_data %>% group_by(Ciclo4, Clima) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ciclo4, "\n", Clima, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Ano e Solo") +
  xlab("")


##### perfil médio dos tipos Solos
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Solo), fun = "mean", geom = "line") + 
  theme_light()


##### perfil médio do Ciclo de Colheita
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(colour = Ciclo4), fun = "mean", geom = "line") + 
  theme_light()


# boxplot dos 95 níveis de Cultivar
ggplot(soybean_data, aes(x = Cultivar, y = kgha)) + 
  geom_boxplot() + theme_light() + theme(axis.text.x = element_blank())


# gráfico em painéis por Solo e Ciclo
ggplot(soybean_data, aes(x = Caracteristica, y = kgha)) + 
  geom_boxplot() + facet_grid(Solo~Ciclo4) +
  theme_light()


# coeficiente de variacao por experimento
cv = soybean_data |> group_by(id) |> summarise(cv(kgha))



# Regression Models -------------------------------------------------------


# linear model 1
mod1.lm <- lm(formula = kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod1.lm)
plot(mod1.lm)

# Box-Cox response Transformation
bc <- MASS::boxcox(mod1.lm)
lambda <- bc$x[which.max(bc$y)]
soybean_data$kghaT <- (soybean_data$kgha^lambda - 1)/lambda

# mod with new variables
mod2.lm <- lm(kgha ~ Solo + Ciclo4 + Clima + PRECTOTCORR_MEAN + 
            T2M_MEAN + WS2M_MEAN + TQV_MEAN, data = DADOS.FINAL)
summary(mod2.lm)
shapiro.test(residuals(object = mod2.lm, type = "pearson"))


# modelo misto com interações triplas covars Solo, Ciclo4, Caracteristica
mod11.lme <- lme(fixed = kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                random = ~1|Cultivar)
summary(mod11.lme)
shapiro.test(residuals(mod11.lme, type = "normalized"))
tseries::jarque.bera.test(residuals(mod11.lme, type = "normalized"))
car::leveneTest(residuals(mod11.lme) ~ Cultivar)


# mod misto triplo com covars Solo, Ciclo4, Clima
mod12.lme <- lme(fixed = kgha ~ Solo*Ciclo4*Clima, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod12.lme)
shapiro.test(residuals(mod12.lme, type = "pearson"))


# mod misto triplo com covars Solo, Ciclo3, Caracteristica
mod13.lme <- lme(fixed = kgha ~ Solo*Ciclo3*Caracteristica, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod13.lme)
shapiro.test(residuals(mod13.lme, type = "pearson"))


# mod misto triplo com covars Solo, Ciclo3, Clima
mod14.lme <- lme(fixed = kgha ~ Solo*Ciclo3*Clima, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod14.lme)
shapiro.test(residuals(mod14.lme, type = "pearson"))


# modelo misto duplo com covars Solo, Ciclo4, Caracteristica
mod21.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod21.lme)
shapiro.test(residuals(mod21.lme, type = "pearson"))
tseries::jarque.bera.test(residuals(mod21.lme, type = "pearson"))
car::leveneTest(residuals(mod21.lme) ~ Cultivar)


# mod misto duplo com covars Solo, Ciclo4, Clima
mod22.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Clima + Ciclo4*Clima,
                 data = soybean_data, random = ~1|Cultivar)
summary(mod22.lme)
shapiro.test(residuals(mod22.lme, type = "pearson"))


# mod misto duplo com covars Solo, Ciclo3, Caracteristica
mod23.lme <- lme(fixed = kgha ~ Solo*Ciclo3 + Solo*Caracteristica + Ciclo3*Caracteristica,
                 data = soybean_data, random = ~1|Cultivar)
summary(mod23.lme)
shapiro.test(residuals(mod23.lme, type = "pearson"))


# mod misto duplo com covars Solo, Ciclo3, Clima ----- melhor em termos de coeficiente
mod24.lme <- lme(fixed = kgha ~ Solo*Ciclo3 + Solo*Clima + Ciclo3*Clima,
                 data = soybean_data, random = ~1|Cultivar)
summary(mod24.lme)
shapiro.test(residuals(object = mod24.lme, type = "pearson"))


# mod misto duplo com novas covars climáticas
mod25.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Clima + Solo*RH2M_MEAN +
                   Solo*T2M_MEAN + Solo*WS2M_MEAN + Ciclo4*Clima + Ciclo4*RH2M_MEAN +
                   Ciclo4*T2M_MEAN + Ciclo4*WS2M_MEAN + Clima*RH2M_MEAN + 
                   Clima*T2M_MEAN + Clima*WS2M_MEAN + RH2M_MEAN*T2M_MEAN + 
                   RH2M_MEAN*WS2M_MEAN + T2M_MEAN*WS2M_MEAN, random = ~1|Cultivar,
                 data = soybean_data)
summary(mod25.lme)
shapiro.test(residuals(object = mod25.lme, type = "pearson"))


# mod de efeitos principais com covars climáticas
mod31.lme <- lme(fixed = kgha ~ Solo + Ciclo4 + Clima + PRECTOTCORR_MEAN + 
                   T2M_MAX + T2M_MIN + RH2M_MEAN + WS2M_MEAN, 
                 data = soybean_data, random = ~1|Cultivar)
summary(mod31.lme)
shapiro.test(residuals(object = mod31.lme, type = "pearson"))


# mod gls duplo covars Solo, Ciclo4, Caracteristica
mod1.gls <- gls(model = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod1.gls)
tseries::jarque.bera.test(residuals(object = mod1.gls, type = "pearson"))
shapiro.test(residuals(object = mod1.gls, type = "pearson"))
boxplot(resid(mod1.gls, type = "normalized") ~ Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo GLS triplo Var no Cultivar")


# mod gls duplo covars Solo, Ciclo3, Clima
mod2.gls <- gls(model = kgha ~ Solo*Ciclo3 + Solo*Clima + Ciclo3*Clima, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod2.gls)
tseries::jarque.bera.test(residuals(object = mod2.gls, type = "normalized"))
shapiro.test(residuals(object = mod2.gls, type = "pearson"))
boxplot(resid(mod2.gls, type = "normalized") ~ Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo GLS triplo Var no Cultivar")


# mod gls duplo com covars climáticas
mod3.gls <- gls(model =  kgha ~ Solo*Ciclo4 + Solo*Clima + Solo*RH2M_MEAN +
                  Solo*T2M_MEAN + Solo*WS2M_MEAN + Ciclo4*Clima + Ciclo4*RH2M_MEAN +
                  Ciclo4*T2M_MEAN + Ciclo4*WS2M_MEAN + Clima*RH2M_MEAN + 
                  Clima*T2M_MEAN + Clima*WS2M_MEAN + RH2M_MEAN*T2M_MEAN + 
                  RH2M_MEAN*WS2M_MEAN + T2M_MEAN*WS2M_MEAN, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod3.gls)
shapiro.test(residuals(object = mod3.gls, type = "pearson"))


# Hypothesis Testing -------------------------------------------------------


# testando os não significantes do mod24.lme
# id dos coeficientes a serem testados
coefID <- c(7,10,11,14)
coefNAM <- names(fixef(mod24.lme)[coefID])
# matriz de contrastes
Cmatrix <- matrix(0, 4, 14)
Cmatrix[cbind(1:4,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
# TESTE DA HIPÓTESE LINEAR GERAL PARA 𝑯𝟎: 𝐂𝜷 = 0
GLH <-  multcomp::glht(model = mod24.lme, linfct = Cmatrix)
summary(GLH) # não significativos


# testando os não significativos do mod21.lme
coefID <- c(7,10,12,13)
coefNAM <- names(fixef(mod21.lme)[coefID])
Cmatrix <- matrix(0,4,13)
Cmatrix[cbind(1:4,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod21.lme, linfct = Cmatrix)
summary(GLH) # não significativos


# testando os não significativos no mod1.gls
coefID <- c(14,15)
coefNAM <- names(coef(mod1.gls)[coefID])
Cmatrix <- matrix(0, 2, 16)
Cmatrix[cbind(1:2,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod1.gls, linfct = Cmatrix)
summary(GLH)



# Model Diagnosis -----------------------------------------------------


# MODELO mod24.lme 

# Resíduos vs Valores Ajustados
plot(mod24.lme)
# Q-Qplot dos resíduos
qqnorm(mod24.lme)
# plot do ajustado pelo observado
plot(mod24.lme, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod24.lme, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod24.lme, Cultivar~resid(., type = "p"), abline = 0, xlim = c(-4.5,4.5))
# Resíduos vs Valores Ajustados por Solo
plot(mod24.lme, resid(., type = "p")~fitted(.)|Solo)

# tests against heteroskedasticity to mod24.lme
residuos <- residuals(object = mod24.lme, type = "pearson")
lmtest::bptest(residuos~Grupo, studentize = FALSE)
bartlett.test(residuos ~ soybean_data$Grupo)
car::leveneTest(residuals(mod24.lme) ~ soybean_data$Grupo)
shapiro.test(residuals(mod24.lme))
tseries::jarque.bera.test(residuals(mod24.lme))

# pacote de gráficos de diagnóstico de modelos
sjPlot::plot_model(mod24.lme, type = "diag")


# MODELO mod21.lme 

# Resíduos vs Valores Ajustados
plot(mod21.lme)
# Q-Qplot dos resíduos
qqnorm(mod21.lme)
# plot do ajustado pelo observado
plot(mod21.lme, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod21.lme, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod21.lme, Cultivar~resid(., type = "p"), abline = 0, xlim = c(-4.5,4.5))
# Resíduos vs Valores Ajustados por Solo
plot(mod21.lme, resid(., type = "p")~fitted(.)|Solo)

# tests against heteroskedasticity to mod5
bartlett.test(residuals(mod21.lme) ~ soybean_data$Grupo)
lmtest::bptest(residuals(mod21.lme) ~ soybean_data$Grupo, studentize = FALSE)
car::leveneTest(residuals(mod21.lme) ~ soybean_data$Grupo)
shapiro.test(residuals(mod21.lme))



# Classification Models ---------------------------------------------------

# k-means Unsupervised Learning of soybean produtivity
set.seed(25)
km <- kmeans(x = soybean_data[,"kgha"], centers = 3)
km$size
km$centers
soybean_data$kgha_km <- factor(km$cluster)


# clustering by quantile
quantile(soybean_data$kgha, probs = seq(0,1,1/3))
soybean_data$kgha_quant = cut(x = kgha, breaks = c(-Inf, 3662, 4453, Inf), 
                          labels = FALSE)

# RANGE da produtividade por cluster
soybean_data |> select(kgha, kgha_km) |> 
  group_by(kgha_km) |> summarise_all(.funs = c(min, max))


# Spliting the data into training and test datasets
set.seed(25)
trainId <- sample(x = 1:nrow(soybean_data), size = nrow(soybean_data)*0.7)

train.dt <- soybean_data |>
  slice(trainId) 

test.dt <- soybean_data |>
  slice(-trainId) 


# mod decision tree model with y = Solo
mod1.dt <- rpart(formula = kgha_km ~ Solo + Ciclo4 + Clima, 
                 data = train.dt, method = "class")

# decision tree plot
rpart.plot(x = mod1.dt, type = 5)
mod1.dt$variable.importance


# mod decision tree model with y = Solo
mod2.dt <- rpart(formula = kgha_km ~ Solo + Ciclo4 + Clima + T2M_MEAN + RH2M_MEAN + WS2M_MEAN, 
                 data = train.dt, method = "class")

# decision tree plot
rpart.plot(x = mod2.dt, type = 5)
mod2.dt$variable.importance
predictions <- predict(object = mod2.dt, newdata = test.dt, type = "class")
library(caret)
CM <- confusionMatrix(data = predictions, reference = test.dt$kgha_km)
CM


# mod floresta aleatória
library(randomForest)
mod1.rf <- randomForest(formula = kgha_km ~ Solo + Ciclo4 + T2M_MEAN + RH2M_MEAN + WS2M_MEAN,
                        data = train.dt, importance = TRUE, ntree = 100)
mod1.rf$importance
predictions.rf <- predict(object = mod1.rf, newdata = test.dt)
CM.rf <- confusionMatrix(data = predictions.rf, reference = test.dt$kgha_km)
CM.rf
