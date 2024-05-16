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
  

# row number
# soybean_data <- soybean_data |> mutate(nrow = 1:nrow(soybean_data))
# # find outliers function
# findoutlier <- function(x) {
#   return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
# }
# # applying find outliers function in groupped observations
# soybean_data <- soybean_data %>%
#   group_by(Caracteristica, Solo, Ciclo4) %>%
#   mutate(outlier = ifelse(findoutlier(kgha), numer, NA))
# # filtrando os indíviduos suspeitos de serem outliers
# filtered_data <- soybean_data |>
#   filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "BMXBonus") |>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "RK7518IPRO")) |>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "BMXExtremaIPRO")) |>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "BRSGO7755RR"))|>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "CZ37B43IPRO")) |>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "DM80I79IPRO"))|>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "DM82I78IPRO")) |>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Latossolo" & Cultivar == "P98Y21IPRO"))  |>
#   bind_rows(soybean_data |> filter(Ano == 2021 & Solo == "Plintossolo" & Cultivar == "CZ37B43IPRO"))|>
#   bind_rows(soybean_data |> filter(Ano == 2020 & Solo == "Latossolo" & Cultivar == "M8644IPRO")) |>
#   bind_rows(soybean_data |> filter(Ano == 2020 & Solo == "Latossolo" & Cultivar == "M8644IPRO"))|>
#   bind_rows(soybean_data |> filter(Ano == 2020 & Solo == "Latossolo" & Cultivar == "W791")) |>
#   bind_rows(soybean_data |> filter(Local == "Aparecida do Rio Negro" & Ano == 2018 & Solo == "Latossolo" & Cultivar == "BRS230068"))
# write.csv(x = filtered_data, file = "Produtividade_Soja_Outliers.csv")
# outliers <- c(240,325,250,252,265,269,285,289,316,374,498,499,547,770)

# excluding outliers from the dataset
# soybean_data <- soybean_data[-c(265, 289, 316, 374, 770),]


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
sample_size = soybean_data %>% group_by(Ciclo4, Caracteristica) %>% summarize(num=n())

soybean_data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ciclo4, "\n", Caracteristica, "\n", "n=", num)) %>%
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


##### perfil médio dos Solos ####
g1 <- ggplot(data = soybean_data, mapping = aes(x = Ciclo, y = kgha)) +
  stat_summary(aes(colour = Car, linetype = Solo), fun = "mean", geom = "line") + 
  theme_light()

##### perfil médio da Caracteristica ####
g2 <- ggplot(data = soybean_data, mapping = aes(x = Ciclo, y = kgha)) +
  stat_summary(aes(colour = Caracteristica), fun = "mean", geom = "line") + 
  theme_light()

##### perfil médio do Grupo 4 do Ciclo de Maturidade ####
g3 <- ggplot(data = soybean_data, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun = "mean", geom = "line", aes(colour = Ciclo4)) +
  scale_x_continuous(breaks = unique(Ano)) +
  scale_color_brewer(palette="Set1") + theme_light()

ggplot(soybean_data, aes(x = Cultivar, y = kgha)) + 
  geom_boxplot() + theme_light() + theme(axis.text.x = element_blank())

ggplot(soybean_data, aes(x = Caracteristica, y = kgha)) + 
  geom_violin() + theme_light()

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
summary(mod1.lme)
tseries::jarque.bera.test(residuals(mod1.lme))
car::leveneTest(residuals(mod1.lme) ~ Cultivar)


# mod misto triplo com covars Solo, Ciclo4, Clima
mod12.lme <- lme(fixed = kgha ~ Solo*Ciclo4*Clima, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod11.lme)


# mod misto duplo com covars Solo, Ciclo3, Caracteristica
mod13.lme <- lme(fixed = kgha ~ Solo*Ciclo3*Caracteristica, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod11.lme)


# mod misto duplo com covars Solo, Ciclo3, Clima
mod14.lme <- lme(fixed = kgha ~ Solo*Ciclo3*Clima, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod11.lme)



# modelo misto duplo com covars Solo, Ciclo4, Caracteristica
mod21.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod2.lme)
tseries::jarque.bera.test(residuals(mod2.lme))
car::leveneTest(residuals(mod2.lme) ~ Cultivar)


# mod misto duplo com covars Solo, Ciclo4, Clima
mod22.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Clima + Ciclo4*Clima,
                 data = soybean_data, random = ~1|Cultivar)
summary(mod21.lme)
shapiro.test(resid(mod21.lme))


# mod misto duplo com covars Solo, Ciclo3, Caracteristica
mod23.lme <- lme(fixed = kgha ~ Solo*Ciclo3 + Solo*Caracteristica + Ciclo3*Caracteristica,
                 data = soybean_data, random = ~1|Cultivar)
summary(mod22.lme)
shapiro.test(resid(mod22.lme))


# mod misto duplo com covars Solo, Ciclo3, Clima
mod24.lme <- lme(fixed = kgha ~ Solo*Ciclo3 + Solo*Clima + Ciclo3*Clima,
                 data = soybean_data, random = ~1|Cultivar)
summary(mod23.lme)
shapiro.test(resid(mod23.lme))






# Hypothesis Testing -------------------------------------------------------


# testando os não significantes do mod2
# id dos coeficientes a serem testados
coefID <- c(7,10,12,13)
coefNAM <- names(fixef(mod2.lme)[coefID])

# matriz de contrastes
Cmatrix <- matrix(0, 4, 13)
Cmatrix[cbind(1:4,coefID)] <- 1
rownames(Cmatrix) <- coefNAM

# TESTE DA HIPÓTESE LINEAR GERAL PARA 𝑯𝟎: 𝐂𝜷 = 0
GLH <-  multcomp::glht(model = mod2.lme, linfct = Cmatrix)
summary(GLH) # não significativos


# testando os não significativos do mod3
coefID <- c(7,11,12)
coefNAM <- names(fixef(mod3.lme)[coefID])
Cmatrix <- matrix(0,3,12)
Cmatrix[cbind(1:3,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod3.lme, linfct = Cmatrix)
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


# pacote de gráficos de diagnóstico de modelos
sjPlot::plot_model(mod4.lme, type = "diag")
