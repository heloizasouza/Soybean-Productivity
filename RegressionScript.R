rm(list = ls())

# Libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(sp)
library(nlme)
library(lme4)

# Data Treatment ----------------------------------------------------------

# loading principal data file
main_data <- read_excel("Data/Dados - Produtividade em Latossolo e Plintossolo - 2018 a 2023.xlsx")
# correcting col names
colnames(main_data) <- gsub(pattern = '\\s', replacement = '\\_', x = names(main_data))

soybean_data <- main_data |>
  # removing accents
  # removendo acentos
  mutate_at(vars(Local, Textura_do_solo, Cultivar), 
            ~stringi::stri_trans_general(.,"Latin-ASCII")) |>
  # transforming categorical variables
  # transformando variáveis categóricas
  mutate_at(vars(Local,Solo,Cultivar, Textura_do_solo), as.factor) |>
  mutate(Anoc = as.factor(Ano)) |>
  #transforming the Date covariate
  # transformando a covariável de Data
  mutate_at(vars(Plantio), as.Date)
  

# correcting coordinates
soybean_data <- soybean_data |>
  separate(Lat._e_Long., into = c("Latit", "Longit"), sep = ";") |>
  mutate(Latit = case_when(
    Local == "Pium" ~ "10°12' 58'' S",
    Local == "Paraiso do Tocantins" ~ "10°11' 16.9'' S",
    Local == "Pedro Afonso" ~ "08°58' 03'' S",
    Local == "Aparecida do Rio Negro" ~ "09°57' 07'' S",
    Local == "Lagoa da Confusao" ~ "10°47' 37'' S",
    TRUE ~ as.character(Latit)
  ),
  Longit = case_when(
    Local == "Pium" ~ "49°15' 1.4'' W",
    Local == "Paraiso do Tocantins" ~ "48°40' 54.6'' W",
    Local == "Goiania" ~ "49°30' 11'' W",
    Local == "Pedro Afonso" ~ "48°10' 29'' W",
    Local == "Aparecida do Rio Negro" ~ "47°58' 19'' W",
    Local == "Lagoa da Confusao" ~ "49°37' 25'' W",
    TRUE ~ as.character(Longit)
  )) |>
  mutate(Latitude = char2dms(from = Latit,chd = "°", chm = "'", chs = "''") 
         |> as.numeric()) |>
  mutate(Longitude = char2dms(from = Longit,chd = "°", chm = "'", chs = "''")
         |> as.numeric())

# correcting planting dates
# corrigindo as datas de plantio
soybean_data <- soybean_data |>
  mutate(Plantio = case_when(
    Plantio == "2022-01-17" ~ as.Date("2022-11-17"),
    Plantio == "2021-11-23" ~ as.Date("2022-11-23"),
    TRUE ~ as.Date(Plantio)),
    Colheita = Plantio+Ciclo)

# creating the harvest cycle categorical covariate
# criando a covariável categórica de ciclo de colheita
soybean_data <- soybean_data |>
  mutate(Ciclo3 = cut(x = Ciclo, breaks = c(-Inf, 105,115,Inf),
                      labels = c("Precoce","Medio","Tardio"))) |>
  mutate(Ciclo4 = cut(x = Ciclo, breaks = c(-Inf, 100, 110, 120, Inf), 
                      labels = c("Super Precoce","Precoce","Medio","Tardio"))) |>
  # creating an identifier
  mutate(idCombi = as.factor(paste(Local, Ano, Ciclo4, sep = "_")),
         id = as.numeric(idCombi))

# creating oceanic season variable
soybean_data <- soybean_data |>
  mutate(Temp = case_when(
    year(Plantio) == 2017 & month(Plantio) == 11 & year(Colheita) == 2018 & month(Colheita) == 3 ~ mean(-0.7,-0.8,-1.0,-0.9,-0.9,-0.7,-0.5),
    year(Plantio) == 2017 & month(Plantio) == 11 & year(Colheita) == 2018 & month(Colheita) == 4 ~ mean(-0.7,-0.8,-1.0,-0.9,-0.9,-0.7,-0.5,-0.2),
    year(Plantio) == 2017 & month(Plantio) == 11 & year(Colheita) == 2018 & month(Colheita) == 2 ~ mean(-0.7,-0.8,-1.0,-0.9,-0.9,-0.7),
    year(Plantio) == 2017 & month(Plantio) == 12 & year(Colheita) == 2018 & month(Colheita) == 3 ~ mean(-0.8,-1.0,-0.9,-0.9,-0.7,-0.5),
    year(Plantio) == 2017 & month(Plantio) == 12 & year(Colheita) == 2018 & month(Colheita) == 4 ~ mean(-0.8,-1.0,-0.9,-0.9,-0.7,-0.5,-0.2),
    year(Plantio) == 2019 & month(Plantio) == 11 & year(Colheita) == 2020 & month(Colheita) == 2 ~ mean(0.3,0.5,0.5,0.5,0.5,0.4),
    year(Plantio) == 2019 & month(Plantio) == 11 & year(Colheita) == 2020 & month(Colheita) == 3 ~ mean(0.3,0.5,0.5,0.5,0.5,0.4,0.2),
    year(Plantio) == 2020 & month(Plantio) == 11 & year(Colheita) == 2021 & month(Colheita) == 2 ~ mean(-1.2,-1.3,-1.2,-1.0,-0.9,-0.8),
    year(Plantio) == 2020 & month(Plantio) == 11 & year(Colheita) == 2021 & month(Colheita) == 3 ~ mean(-1.2,-1.3,-1.2,-1.0,-0.9,-0.8,-0.7),
    year(Plantio) == 2021 & month(Plantio) == 10 & year(Colheita) == 2022 & month(Colheita) == 2 ~ mean(-0.7,-0.8,-1.0,-1.0,-1.0,-0.9,-1.0),
    year(Plantio) == 2021 & month(Plantio) == 11 & year(Colheita) == 2022 & month(Colheita) == 3 ~ mean(-0.8,-1.0,-1.0,-1.0,-0.9,-1.0,-1.1),
    year(Plantio) == 2022 & month(Plantio) == 11 & year(Colheita) == 2023 & month(Colheita) == 2 ~ mean(-1.0,-0.9,-0.8,-0.7,-0.4,-0.1),
    year(Plantio) == 2022 & month(Plantio) == 11 & year(Colheita) == 2023 & month(Colheita) == 3 ~ mean(-1.0,-0.9,-0.8,-0.7,-0.4,-0.1,0.2),
  )) |>
  mutate(Caracteristica = cut(Temp, breaks = c(-2, -0.5, 0.5), labels = c("LaNina", "Neutro"))) |>
  mutate(Clima = cut(Temp, breaks = c(-1.5,-1.0,-0.5,0.5), 
                     labels = c("ModerLaNina","FracoLaNina","Neutro"))) |>
  mutate(Clima = ordered(Clima, levels = c("Neutro", "FracoLaNina", "ModerLaNina")))

# creating Group variable of interactions Solo, Ciclo and Caracteristica
soybean_data <- soybean_data |>
  #mutate(tha = kgha/1000) |>
  mutate(Grupo = case_when(
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "LaNina" ~ "G1",
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "Neutro" ~ "G2",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "LaNina" ~ "G3",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "Neutro" ~ "G4",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & Caracteristica == "LaNina" ~ "G5",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & Caracteristica == "Neutro" ~ "G6",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & Caracteristica == "LaNina" ~ "G7",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & Caracteristica == "Neutro" ~ "G8",
    Solo == "Latossolo" & Ciclo4 == "Medio" & Caracteristica == "LaNina" ~ "G9",
    Solo == "Latossolo" & Ciclo4 == "Medio" & Caracteristica == "Neutro" ~ "G10",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & Caracteristica == "LaNina" ~ "G11",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & Caracteristica == "Neutro" ~ "G12",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & Caracteristica == "LaNina" ~ "G13",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & Caracteristica == "Neutro" ~ "G14",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & Caracteristica == "LaNina" ~ "G15",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & Caracteristica == "Neutro" ~ "G16",
    .default = "outro"
  )) |>
  mutate(Grupo = factor(Grupo, levels = c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9",
                                          "G10", "G11", "G12", "G13", "G14", "G15", "G16"))) |>
  mutate(Grupo2 = case_when(
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "LaNina" ~ "G1",
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "Neutro" ~ "G2",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "LaNina" ~ "G3",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "Neutro" ~ "G1",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & Caracteristica == "LaNina" ~ "G5",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & Caracteristica == "Neutro" ~ "G6",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & Caracteristica == "LaNina" ~ "G1",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & Caracteristica == "Neutro" ~ "G1",
    Solo == "Latossolo" & Ciclo4 == "Medio" & Caracteristica == "LaNina" ~ "G9",
    Solo == "Latossolo" & Ciclo4 == "Medio" & Caracteristica == "Neutro" ~ "G10",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & Caracteristica == "LaNina" ~ "G11",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & Caracteristica == "Neutro" ~ "G12",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & Caracteristica == "LaNina" ~ "G13",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & Caracteristica == "Neutro" ~ "G14",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & Caracteristica == "LaNina" ~ "G15",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & Caracteristica == "Neutro" ~ "G16",
    .default = "outro"
  )) |>
  mutate(Grupo2 = factor(Grupo2, levels = c("G1", "G2", "G3", "G5", "G6", "G9",
                                          "G10", "G11", "G12", "G13", "G14", "G15", "G16")))
  

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


# Modeling ----------------------------------------------------------------

# linear model 1
mod1.lm <- lm(formula = kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod1.lm)
plot(mod1.lm)

# Box-Cox response Transformation
bc <- MASS::boxcox(mod1.lm)
lambda <- bc$x[which.max(bc$y)]
soybean_data$kghaT <- (soybean_data$kgha^lambda - 1)/lambda



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


