rm(list = ls())

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
  mutate_at(vars(Local,Solo,Cultivar, Textura.do.solo), as.factor) |>
  mutate(Anoc = as.factor(Ano)) |>
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
  mutate(Caracteristica = cut(Temp, breaks = c(-2, -0.5, 0.5, 2), labels = c("LaNina", "Neutro", "ElNino")))

# creating Group variable of interactions Solo, Ciclo and Caracteristica
soybean_data <- soybean_data |>
  mutate(tha = kgha/1000) |>
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
                                          "G10", "G11", "G12", "G13", "G14", "G15", "G16")))


# Descriptive Analysis ----------------------------------------------------

attach(soybean_data)

##### Densidade por tipo de Solo #####
sample_size = soybean_data %>% group_by(Solo) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Solo, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Solo") +
  xlab("")

##### Densidade por ANO #####
sample_size = soybean_data %>% group_by(Ano) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ano, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Ano)) +
  geom_violin(width=1.4) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Ano") +
  xlab("")

##### Densidade por Grupo do Ciclo #####
soybean_data |>
  ggplot(mapping = aes(x = Ciclo4, y = kgha)) +
  geom_violin()

##### Densidade por Grupo do Ciclo e SOLO #####
sample_size = soybean_data %>% group_by(Ciclo4, Solo) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ciclo4, "\n", Solo, "\n", "n=", num)) %>%
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


##### perfil médio do Tipo de Solo ####
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(aes(linetype = Solo, colour = Solo), fun = "mean", geom = "line") +
  scale_x_continuous(breaks = unique(Ano))

##### perfil médio do Grupo 4 do Ciclo de Maturidade ####
ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun = "mean", geom = "line", aes(linetype = Ciclo4, colour = Ciclo4)) +
  scale_x_continuous(breaks = unique(Ano)) +
  scale_color_brewer(palette="Set1")

###### gráfico do perfil médio da Caracteristica
ggplot(data = soybean_data, mapping = aes(x = Ciclo, y = kgha)) +
  stat_summary(aes(colour = Caracteristica) ,fun = "mean", geom = "line")


# Modeling ----------------------------------------------------------------

# mod linear
mod1.lm <- lm(kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod1.lm)
plot(mod1.lm, which=2, col="firebrick")
plot(mod1.lm, which=3, col="firebrick")
lmtest::bptest(mod1.lm)

# mod misto com interação tripla
mod1.lme <- lme(kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data, random = ~1|Cultivar)
summary(mod1.lme)


# mod misto com interações duplas
mod2.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
            data = soybean_data, random = ~1|Cultivar)
summary(mod2.lme)


# mod misto sem interação Solo Caracteristica
mod3.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
            data = soybean_data, random = ~1|Cultivar)
# mod3 <- lmerTest::lmer(kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica + (1|Cultivar), soybean_data)
summary(mod3.lme)


# mod misto com a covar Grupo
# mod4 <- lmerTest::lmer(formula = kgha ~ Grupo + (1|Cultivar), data = soybean_data)
mod4.lme <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod4)

# removendo os níveis não significativos G4, G8
# mod41 <- lmerTest::lmer(kgha ~ Grupo + (1|Cultivar), soybean_data,
#                         subset = !(Grupo %in% c("G4","G8","G11")))
mod41.lme <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar,
             subset = !(Grupo %in% c("G4","G8","G11")))
summary(mod41.lme)

# mod com variável resposta em tonelada
# mod41 <- lmerTest::lmer(tha ~ Grupo + (1|Cultivar), soybean_data)
# summary(mod41)

# mod gls com interação tripla
mod1.gls <- gls(model = kgha ~ Solo*Ciclo4*Caracteristica, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod1.gls)

sum(mod1.gls$coefficients[c(1,2,3,6,7,10,11,14)])
car::leveneTest(residuals(mod1.gls) ~ soybean_data$Cultivar) # rejeita homocedasticidade
tseries::jarque.bera.test(residuals(mod1.gls)) # rejeita normalidade

# mod gls com interações duplas
mod2.gls <- gls(model = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica,
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod2.gls)

# mod gls sem a interação Solo Característica
mod3.gls <- gls(model = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod3.gls)


# Hypothesis Testing -------------------------------------------------------


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
coefNAM <- names(fixef(mod3.lme)[coefID])
Cmatrix <- matrix(0,3,12)
Cmatrix[cbind(1:3,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod3.lme, linfct = Cmatrix)
summary(GLH) # não significativos


# obtendo as esperanças do modelo 3
sum(mod3.lme$coefficients$fixed[c(1)])


# testando os não significativos do mod4
coefID <- c(4,8)
coefNAM <- names(fixef(mod4.lme)[coefID])
Cmatrix <- matrix(0,2,16)
Cmatrix[cbind(1:2,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod4.lme, linfct = Cmatrix)
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


# MODELO 4 

# Resíduos vs Valores Ajustados
plot(mod4.lme)
# Q-Qplot dos resíduos
qqnorm(mod4.lme)
# plot do ajustado pelo observado
plot(mod4.lme, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod4.lme, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod4.lme, Cultivar~resid(., type = "p"), abline = 0, xlim = c(-4.5,4.5))
# Resíduos vs Valores Ajustados por Solo
plot(mod4.lme, resid(., type = "p")~fitted(.)|Solo)


# tests against heteroskedasticity to mod4
residuos <- residuals(mod4.lme)
lmtest::bptest(residuos~Grupo, studentize = FALSE)
bartlett.test(residuos ~ soybean_data$Grupo)
car::leveneTest(residuals(mod4.lme) ~ soybean_data$Grupo)
shapiro.test(residuals(mod4.lme))
tseries::jarque.bera.test(residuals(mod4.lme))

# tests against heteroskedasticity to mod5
bartlett.test(residuals(mod5.gls) ~ soybean_data$Grupo)
lmtest::bptest(residuals(mod5.gls) ~ soybean_data$Grupo, studentize = FALSE)
car::leveneTest(residuals(mod5.gls) ~ soybean_data$Grupo)
shapiro.test(residuals(mod5.gls))


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
plot(mod3.lme)
# Q-Qplot dos resíduos
qqnorm(mod3.lme)
# plot do ajustado pelo observado
plot(mod3.lme, kgha ~ fitted(.))
# Q-Qplot dos efeitos aleatórios
qqnorm(mod3.lme, ~ranef(.))
# boxplot dos resíduos por Cultivar
plot(mod3.lme, Cultivar~resid(., type = "p"), abline = 0)
# Resíduos vs Valores Ajustados por Solo
plot(mod3.lme, resid(., type = "p")~fitted(.)|Solo)



sjPlot::plot_model(mod4.lme, type = "diag")




# Testing -----------------------------------------------------------------

dados <- model.matrix(mod1.gls)