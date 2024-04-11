

# Libraries ---------------------------------------------------------------

library(xlsx)
library(tidyverse)
library(sp)
library(nlme)

# Data Treatment ----------------------------------------------------------

# loading principal data file
main_data <- read.xlsx("Data/Dados - Produtividade em Latossolo e Plintossolo - 2018 a 2023.xlsx", sheetIndex = 1)
main_data <- main_data[,1:12]

soybean_data <- main_data |>
  # removing accents
  # removendo acentos
  mutate_at(vars(Local, Textura.do.solo, Cultivar), 
            ~stringi::stri_trans_general(.,"Latin-ASCII")) |>
  # transforming categorical variables
  # transformando variáveis categóricas
  mutate_at(vars(Local,Ano,Solo,Cultivar, Textura.do.solo), as.factor) |>
  #transforming the Date covariate
  # transformando a covariável de Data
  mutate_at(vars(Plantio), as.Date)
  

# correcting coordinates
soybean_data <- soybean_data |>
  separate(Lat..e.Long., into = c("Latit", "Longit"), sep = ";") |>
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
  mutate(Ciclo4 = cut(x = Ciclo, breaks = c(-Inf, 100, 110, 120, Inf), 
                      labels = c("Super Precoce","Precoce","Medio","Tardio")))

# creating oceanic season variable
soybean_data <- soybean_data |>
  mutate(tempeture = case_when(
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
  mutate(caracteristica = cut(tempeture, breaks = c(-2, -0.5, 0.5, 2), labels = c("laNina", "neutro", "elNino")))


soybean_data <- soybean_data |>
  mutate(tha = kgha/1000) |>
  mutate(Grupo = case_when(
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & caracteristica == "laNina" ~ "G1",
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & caracteristica == "neutro" ~ "G2",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & caracteristica == "laNina" ~ "G3",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & caracteristica == "neutro" ~ "G4",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & caracteristica == "laNina" ~ "G5",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & caracteristica == "neutro" ~ "G6",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & caracteristica == "laNina" ~ "G7",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & caracteristica == "neutro" ~ "G8",
    Solo == "Latossolo" & Ciclo4 == "Medio" & caracteristica == "laNina" ~ "G9",
    Solo == "Latossolo" & Ciclo4 == "Medio" & caracteristica == "neutro" ~ "G10",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & caracteristica == "laNina" ~ "G11",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & caracteristica == "neutro" ~ "G12",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & caracteristica == "laNina" ~ "G13",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & caracteristica == "neutro" ~ "G14",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & caracteristica == "laNina" ~ "G15",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & caracteristica == "neutro" ~ "G16",
    .default = "outro"
  )) |>
  mutate(Grupo = factor(Grupo, levels = c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9",
                                          "G10", "G11", "G12", "G13", "G14", "G15", "G16")))


# Descriptive Analysis ----------------------------------------------------


# gráfico do perfil médio do caracteristica
ggplot(data = soybean_data, mapping = aes(x = Ciclo, y = kgha)) +
  stat_summary(aes(colour = caracteristica) ,fun = "mean", geom = "line")

# gráfico do perfil médio do Solo
ggplot(data = soybean_data, mapping = aes(x = as.numeric(Ano), y = kgha)) +
  stat_summary(aes(colour = Solo), fun = "mean", geom = "line") +
  scale_x_continuous(breaks = unique(as.numeric(Ano)))


# Modeling ----------------------------------------------------------------



# mod com interação tripla
mod1 <- lme(kgha ~ Solo*Ciclo4*caracteristica, data = soybean_data, random = ~1|Cultivar)
summary(mod1)


# mod com interações duplas
mod2 <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*caracteristica + Ciclo4*caracteristica, 
            data = soybean_data, random = ~1|Cultivar)
summary(mod2)


# mod sem interação Solo caracteristica
mod3 <- lme(fixed = kgha ~ Solo*Ciclo4 + Ciclo4*caracteristica, 
            data = soybean_data, random = ~1|Cultivar)
mod3 <- lmerTest::lmer(kgha ~ Solo*Ciclo4 + Ciclo4*caracteristica + (1|Cultivar), soybean_data)
summary(mod3)


# mod com a covar Grupo
mod4 <- lmerTest::lmer(formula = kgha ~ Grupo + (1|Cultivar), data = soybean_data)
mod4 <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod4)

# removendo os níveis não significativos G4, G8
# mod41 <- lmerTest::lmer(kgha ~ Grupo + (1|Cultivar), soybean_data,
#                         subset = !(Grupo %in% c("G4","G8","G11")))
mod41 <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar,
             subset = !(Grupo %in% c("G4","G8","G11")))
summary(mod41)

# mod com variável resposta em tonelada
# mod41 <- lmerTest::lmer(tha ~ Grupo + (1|Cultivar), soybean_data)
# summary(mod41)

# mod usando gls - generalized least squares
mod5.gls <- gls(model = kgha ~ Grupo, data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod5.gls)



# Teste de Hipótese -------------------------------------------------------


# testando os não significantes do mod2
# id dos coeficientes a serem testados
coefID <- c(7,10,12,13)
coefNAM <- names(fixef(mod2)[coefID])

# matriz de contrastes
Cmatrix <- matrix(0, 4, 13)
Cmatrix[cbind(1:4,coefID)] <- 1
rownames(Cmatrix) <- coefNAM

# TESTE DA HIPÓTESE LINEAR GERAL PARA 𝑯𝟎: 𝐂𝜷 = 0
GLH <-  multcomp::glht(model = mod2, linfct = Cmatrix)
summary(GLH) # não significativos


# testando os não significativos do mod3
coefID <- c(7,11,12)
coefNAM <- names(fixef(mod3)[coefID])
Cmatrix <- matrix(0,3,12)
Cmatrix[cbind(1:3,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod3, linfct = Cmatrix)
summary(GLH) # não significativos


# obtendo as esperanças do modelo 3
sum(mod3$coefficients$fixed[c(1)])


# testando os não significativos do mod4
coefID <- c(4,8,11)
coefNAM <- names(fixef(mod4)[coefID])
Cmatrix <- matrix(0,3,16)
Cmatrix[cbind(1:3,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod4, linfct = Cmatrix)
summary(GLH) # não significativos

anova(mod4, mod5.gls)

# Análise de Resíduos -----------------------------------------------------


# MODELO 4 

# Resíduos vs Valores Ajustados
plot(mod4)
# Q-Qplot dos resíduos
qqnorm(mod4)
# plot do ajustado pelo observado
plot(mod4, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod4, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod4, Cultivar~resid(., type = "p"), abline = 0, xlim = c(-4.5,4.5))
# Resíduos vs Valores Ajustados por Solo
plot(mod4, resid(., type = "p")~fitted(.)|Solo)


# MODELO 5 

# Resíduos vs Valores Ajustados
plot(mod5.gls)
# Q-Qplot dos resíduos
qqnorm(mod5.gls)
# plot do ajustado pelo observado
plot(mod5.gls, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod5.gls, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod5.gls, Cultivar~resid(., type = "p"), abline = 0, xlim = c(-4.5,4.5))
# Resíduos vs Valores Ajustados por Solo
plot(mod5.gls, resid(., type = "p")~fitted(.)|Solo)


# MODELO 3 

# Resíduos vs Valores Ajustados
plot(mod3)
# Q-Qplot dos resíduos
qqnorm(mod3)
# plot do ajustado pelo observado
plot(mod3, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod3, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod3, Cultivar~resid(., type = "p"), abline = 0)
# Resíduos vs Valores Ajustados por Solo
plot(mod3, resid(., type = "p")~fitted(.)|Solo)



sjPlot::plot_model(mod4, type = "diag")
