# Script para testes da modelagem do problema


# FINDING OUTLIERS

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




##### Novas covariáveis para modelagem #####

# para ajuste do modelo misto 4 e mod misto 8
# grupos G4 e G8 jogados no intercepto G1
soybean_data <- soybean_data |>
  mutate(Grupo2 = case_when(
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "LaNina" ~ "G1",
    Solo == "Latossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "Neutro" ~ "G2",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "LaNina" ~ "G3",
    Solo == "Plintossolo" & Ciclo4 == "Super Precoce" & Caracteristica == "Neutro" ~ "G4",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & Caracteristica == "LaNina" ~ "G5",
    Solo == "Latossolo" & Ciclo4 == "Precoce" & Caracteristica == "Neutro" ~ "G6",
    Solo == "Plintossolo" & Ciclo4 == "Precoce" & Caracteristica == "LaNina" ~ "G7",
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
  mutate(Grupo2 = factor(Grupo2, levels = c("G1", "G2", "G3", "G5", "G6", "G7", "G9",
                                            "G10", "G11", "G12", "G13", "G14", "G15", "G16")))

# para ajuste do modelo GLS 3
# grupos G4, G7, G8 e G11 jogados no intercepto G1
soybean_data <- soybean_data |>
  mutate(Grupo3 = case_when(
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
    Solo == "Plintossolo" & Ciclo4 == "Medio" & Caracteristica == "LaNina" ~ "G1",
    Solo == "Plintossolo" & Ciclo4 == "Medio" & Caracteristica == "Neutro" ~ "G12",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & Caracteristica == "LaNina" ~ "G13",
    Solo == "Latossolo" & Ciclo4 == "Tardio" & Caracteristica == "Neutro" ~ "G14",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & Caracteristica == "LaNina" ~ "G15",
    Solo == "Plintossolo" & Ciclo4 == "Tardio" & Caracteristica == "Neutro" ~ "G16",
    .default = "outro"
  )) |>
  mutate(Grupo3 = factor(Grupo3, levels = c("G1", "G2", "G3", "G5", "G6", "G9",
                                            "G10", "G12", "G13", "G14", "G15", "G16")))

# para ajuste dos modelos GLS 6 e 7
soybean_data <- soybean_data |>
  group_by(id) |> 
  mutate(prodMean = mean(kgha), prodMed = median(kgha))

# Box-Cox response Transformation
mod1.lm <- lm(kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
bc <- MASS::boxcox(mod1.lm)
lambda <- 0.9
soybean_data$kghaT <- (soybean_data$kgha^lambda - 1)/lambda



#### Gráficos #####

# Definir os modelos e transformações de resposta
modelos <- list(
  list(formula = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, nome = "y=kgha"),
  list(formula = kghaT ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, nome = "y=kghaT"),
  list(formula = log(kgha) ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, nome = "y=log(kgha)")
)
# Iterar sobre os modelos
for (mod in modelos) {
  # Ajustar o modelo linear
  mod1.lm <- lm(mod$formula, data = soybean_data)
  # Realizar o teste de Shapiro-Wilk
  print(shapiro.test(rstandard(mod1.lm)))
  # Calcula a média e o desvio padrão dos resíduos
  mean_resid <- mean(rstandard(mod1.lm))
  sd_resid <- sd(rstandard(mod1.lm))
  # Calcula os graus de liberdade para a distribuição t
  df <- length(rstandard(mod1.lm)) - length(coefficients(mod1.lm))
  # Realizar o teste de Kolmogorov-Smirnov
  residuos <- rstandard(mod1.lm)
  print(ks.test(x = residuos, y = "pt", df = df))
  # Plota o histograma dos resíduos
  hist(residuos, freq = FALSE, main = paste("Resíduos de", mod$nome))
  # Adiciona a curva de densidade da distribuição t
  curve(dt(x, df), add = TRUE, col = "blue", lwd = 2)
  # Adiciona a curva de densidade da distribuição normal
  curve(dnorm(x, mean_resid, sd_resid), add = TRUE, col = "red", lwd = 2)
  # Adiciona a curva de densidade da distribuição gama
  curve(dgamma(x, shape = 2, rate = 1/2), add = TRUE, col = "green", lwd = 2)
}


##### BIBLIOTECAS ####
library(DHARMa)

##### Mod Linear 1 - var resp original ####

# mod linear independente
mod1.lm <- lm(kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
mod1.lm <- lm(kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
              data = soybean_data)
summary(mod1.lm)
# rejeita homocedasticidade
lmtest::bptest(mod1.lm)
# rejeita normalidade
tseries::jarque.bera.test(resid(mod1.lm))

plot(mod1.lm, which=3)
# Plintossolo apresenta maior variabilidade
boxplot(rstandard(mod1.lm) ~ soybean_data$Solo,
        xlab="Solo", ylab="Resíduos", main="Modelo LM triplo Var Independente")
# há maior variabilidade nos Ciclos Médio e Tardio
boxplot(rstandard(mod1.lm) ~ soybean_data$Ciclo4,
        xlab="Ciclo", ylab="Resíduos", main="Modelo LM triplo Var Independente")
# LaNina apresenta maior variabilidade que o Neutro
boxplot(rstandard(mod1.lm) ~ soybean_data$Caracteristica,
        xlab="Caracteristica", ylab="Resíduos", main="Modelo LM triplo Var Independente")
# há uma variabilidade significativa nos diferentes genótipos
boxplot(rstandard(mod1.lm) ~ soybean_data$Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo LM triplo Var Independente")


##### Mod Linear 2 - var resp transformada por Box-Cox ####

# transformação Box-Cox
bc <- MASS::boxcox(mod1.lm)
lambda <- bc$x[which.max(bc$y)]
soybean_data$kghaT <- (soybean_data$kgha^lambda - 1)/lambda

# mod linear com a var resposta transformada
mod2.lm <- lm(kghaT ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod2.lm)
lmtest::bptest(mod2.lm) # heterocedasticidade residual
tseries::jarque.bera.test(resid(mod2.lm)) # residuos normais


##### Mod MISTO 1 - Resp original interação tripla e Cultivar de efeit aleat ####

mod1.lme <- lme(fixed = kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                random = ~1|Cultivar)
summary(mod1.lme)
tseries::jarque.bera.test(resid(mod1.lme)) # rejeita normalidade
shapiro.test(resid(mod1.lme))
car::leveneTest(resid(mod1.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 2 - Resp original interações duplas e Cultivar de efeit aleat ####

mod2.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod2.lme)
tseries::jarque.bera.test(resid(mod2.lme)) # rejeita normalidade
shapiro.test(resid(mod2.lme))
car::leveneTest(resid(mod2.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 3 - Resp original covar Grupo e Cultivar de efeit aleat ####

mod3.lme <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod3.lme)
tseries::jarque.bera.test(resid(mod3.lme)) # rejeita normalidade
shapiro.test(resid(mod3.lme))
car::leveneTest(resid(mod3.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 4 - Resp original covar Grupo2 e Cultivar de efeit aleat ####

mod4.lme <- lme(fixed = kgha ~ Grupo2, data = soybean_data, random = ~1|Cultivar)
summary(mod4.lme)
tseries::jarque.bera.test(resid(mod4.lme)) # rejeita normalidade
shapiro.test(resid(mod4.lme))
car::leveneTest(resid(mod4.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 5 - Resp transform por Box-Cox interação tripla e Cultivar de efeit aleat ####

mod5.lme <- lme(fixed = kghaT ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                random = ~1|Cultivar)
summary(mod5.lme)
tseries::jarque.bera.test(resid(mod5.lme)) # rejeita normalidade
shapiro.test(resid(mod5.lme))
car::leveneTest(resid(mod5.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 6 - Resp transform por Box-Cox interações duplas e Cultivar de efeit aleat ####

mod6.lme <- lme(fixed = kghaT ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod6.lme)
tseries::jarque.bera.test(resid(mod6.lme)) # rejeita normalidade
shapiro.test(resid(mod6.lme))
car::leveneTest(resid(mod6.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 7 - Resp transform por Box-Cox sem interações duplas de Solo e Característica ####

mod7.lme <- lme(fixed = kghaT ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod7.lme)
tseries::jarque.bera.test(resid(mod7.lme)) # rejeita normalidade
shapiro.test(resid(mod7.lme))
car::leveneTest(resid(mod7.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 8 - Resp transform por Box-Cox covar Grupo e Cultivar de efeit aleat ####

mod8.lme <- lme(fixed = kghaT ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod8.lme)
tseries::jarque.bera.test(resid(mod8.lme)) # rejeita normalidade
shapiro.test(resid(mod8.lme))
car::leveneTest(resid(mod8.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 9 - Resp transform por Box-Cox covar Grupo2 e Cultivar de efeit aleat ####

mod9.lme <- lme(fixed = kghaT ~ Grupo2, data = soybean_data, random = ~1|Cultivar)
summary(mod9.lme)
tseries::jarque.bera.test(resid(mod9.lme)) # rejeita normalidade
shapiro.test(resid(mod9.lme))
car::leveneTest(resid(mod9.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 10 - Resp transform kgha^0.47 interação tripla e Cultivar de efeit aleat ####

mod10.lme <- lme(fixed = (kgha)^0.47 ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                random = ~1|Cultivar)
summary(mod10.lme)
tseries::jarque.bera.test(resid(mod10.lme)) # rejeita normalidade
shapiro.test(resid(mod10.lme))
car::leveneTest(resid(mod10.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 11 - Resp transform kgha^0.47 interações duplas e Cultivar de efeit aleat ####

mod11.lme <- lme(fixed = (kgha)^0.47 ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod11.lme)
tseries::jarque.bera.test(resid(mod11.lme)) # rejeita normalidade
shapiro.test(resid(mod11.lme))
car::leveneTest(resid(mod11.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 12 - Resp transform kgha^0.47 sem interações duplas de Solo e Característica ####

mod12.lme <- lme(fixed = kgha^0.4 ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod12.lme)
tseries::jarque.bera.test(resid(mod12.lme)) # rejeita normalidade
shapiro.test(resid(mod12.lme))
car::leveneTest(resid(mod12.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade
ks.test(resid(mod12.lme), "dnorm")


##### Mod MISTO 13 - Resp transform kgha^0.47 covar Grupo e Cultivar de efeit aleat ####

mod13.lme <- lme(fixed = (kgha)^0.47 ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod13.lme)
tseries::jarque.bera.test(resid(mod13.lme)) # rejeita normalidade
shapiro.test(resid(mod13.lme))
car::leveneTest(resid(mod13.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 14 - Resp transform kgha^0.47 covar Grupo2 e Cultivar de efeit aleat ####

mod14.lme <- lme(fixed = (kgha)^0.47 ~ Grupo2, data = soybean_data, random = ~1|Cultivar)
summary(mod14.lme)
tseries::jarque.bera.test(resid(mod14.lme)) # rejeita normalidade
shapiro.test(resid(mod14.lme))
car::leveneTest(resid(mod14.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 15 - Resp transform log(kgha) interação tripla e Cultivar de efeit aleat ####

mod15.lme <- lme(fixed =  log(kgha) ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                 random = ~1|Cultivar)
summary(mod15.lme)
tseries::jarque.bera.test(resid(mod15.lme)) # rejeita normalidade
shapiro.test(resid(mod15.lme))
car::leveneTest(resid(mod15.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 16 - Resp transform  log(kgha) interações duplas e Cultivar de efeit aleat ####

mod16.lme <- lme(fixed =  log(kgha) ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                 data = soybean_data, random = ~1|Cultivar)
summary(mod16.lme)
tseries::jarque.bera.test(resid(mod16.lme)) # rejeita normalidade
shapiro.test(resid(mod16.lme))
car::leveneTest(resid(mod16.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 17 - Resp transform  log(kgha) sem interações duplas de Solo e Característica ####

mod17.lme <- lme(fixed =  log(kgha) ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                 data = soybean_data, random = ~1|Cultivar)
summary(mod17.lme)
tseries::jarque.bera.test(resid(mod17.lme)) # rejeita normalidade
shapiro.test(resid(mod17.lme))
car::leveneTest(resid(mod17.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 18 - Resp transform  log(kgha) covar Grupo e Cultivar de efeit aleat ####

mod18.lme <- lme(fixed =  log(kgha) ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod18.lme)
tseries::jarque.bera.test(resid(mod18.lme)) # rejeita normalidade
shapiro.test(resid(mod18.lme))
car::leveneTest(resid(mod18.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 19 - Resp transform  log(kgha) covar Grupo2 e Cultivar de efeit aleat ####

mod19.lme <- lme(fixed =  log(kgha) ~ Grupo2, data = soybean_data, random = ~1|Cultivar)
summary(mod19.lme)
tseries::jarque.bera.test(resid(mod19.lme)) # rejeita normalidade
shapiro.test(resid(mod19.lme))
car::leveneTest(resid(mod19.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade



##### Mod GLS 1 - Resp original interaç tripla e Variância no Cultivar ####

mod1.gls <- gls(model = kgha ~ Solo*Ciclo4*Caracteristica, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod1.gls)
tseries::jarque.bera.test(resid(mod1.gls)) # rejeita normalidade
boxplot(resid(mod1.gls, type = "normalized") ~ Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo GLS triplo Var no Cultivar")


##### Mod GLS 2 - Resp original covar Grupo e Variância no Cultivar ####

mod2.gls <- gls(model = kgha ~ Grupo, data = soybean_data, 
                weights = varIdent(form = ~1|Cultivar))
summary(mod2.gls)
tseries::jarque.bera.test(resid(mod2.gls)) # rejeita normalidade
boxplot(resid(mod2.gls, type = "normalized") ~ Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo GLS covar Grupo e Var no Cultivar")


##### Mod GLS 3 - Resp original covar Grupo3 e Variância no Cultivar ####

mod3.gls <- gls(model = kgha ~ Grupo3, data = soybean_data, 
                weights = varIdent(form = ~1|Cultivar))
summary(mod3.gls)
tseries::jarque.bera.test(resid(mod3.gls)) # rejeita normalidade
boxplot(resid(mod3.gls, type = "normalized") ~ Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo GLS var Resp Transformada")


##### Mod GLS 4 - Resp transform por Box-Cox inter tripla e Var no Cultivar ####

mod4.gls <- gls(model = kghaT ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                weights = varIdent(form = ~1|Cultivar))
summary(mod4.gls)
tseries::jarque.bera.test(resid(mod4.gls)) # rejeita normalidade
boxplot(resid(mod4.gls, type = "normalized") ~ Cultivar,
        xlab="Cultivar", ylab="Resíduos", main="Modelo GLS var Resp Transformada")


##### Mod GLS 5 - Resp transform por Box-Cox covar Grupo2 e Var no Cultivar ####

mod5.gls <- gls(model = kghaT ~ Grupo2, data = soybean_data,
                weights = varIdent(form = ~1|Cultivar))
summary(mod5.gls)
tseries::jarque.bera.test(resid(mod5.gls)) # rejeita normalidade
car::leveneTest(resid(mod5.gls) ~ Solo*Ciclo4*Caracteristica) # rejeita homocedasticidade


##### Mod GLS 6 - Resp Produtividade Média por id inter tripla e Var Independente ####

mod6.gls <- gls(model = prodMean ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod6.gls)
tseries::jarque.bera.test(resid(mod6.gls)) # rejeita normalidade
car::leveneTest(resid(mod6.gls) ~ Solo*Ciclo4*Caracteristica) # rejeita homocedasticidade


##### Mod GLS 7 - Resp Produtiv Média covar Grupo e Var Independente  ####

mod7.gls <- gls(model = prodMean ~ Grupo, data = soybean_data)
summary(mod7.gls)
tseries::jarque.bera.test(resid(mod7.gls)) # rejeita normalidade
car::leveneTest(resid(mod7.gls) ~ Solo*Ciclo4*Caracteristica) # rejeita homocedasticidade


##### Mod GLS 8 - Resp Produtividade Média por id inter tripla e Var no Cultivar ####

mod8.gls <- gls(model = prodMean ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                weights = varIdent(form = ~1|Cultivar))
summary(mod8.gls) # o modelo não ajusta


##### Mod GLS 9 - Resp Produtiv Média covar Grupo e Var no Cultivar ####

mod9.gls <- gls(model = prodMean ~ Grupo, data = soybean_data,
                weights = varIdent(form = ~1|Cultivar))
summary(mod9.gls) # o modelo não ajusta


##### Mod GLS 10 - Resp Produtiv Mediana covar Grupo e Var Cultivar  ####

mod10.gls <- gls(model = prodMed ~ Grupo, data = soybean_data)
summary(mod10.gls)
tseries::jarque.bera.test(resid(mod10.gls)) # rejeita normalidade



##### Mod GLMM 1 - Resp original interaç tripla e Cultivar randon ####

mod1.glmm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "identity"),
                   weights = varIdent(1|Cultivar))
summary(mod1.glmm)
shapiro.test(mod1.glmm@u)
shapiro.test(simulationOutput)

shapiro.test(resid(mod1.glmm, type = "pearson"))


qqplot(y = resid(mod1.glmm, type="pearson"))

# obtém os resíduos escalonados simulados
simulationOutput <- simulateResiduals(fittedModel = mod1.glmm, , quantileFunction = qnorm, outlierValues = c(-7,7))

# testa a uniformidade geral dos resíduos simulados 
# pelo ks.test e compara com a dist uniforme 
testUniformity(simulationOutput, plot = F)
#ks.test(simulationOutput$scaledResiduals, "dunif") # seria algo tipo isso

# faz testes baseados em simulação para sobre/subdispersão 
# e verifica se é igual a observada
testDispersion(simulationOutput, plot = F)

# A função ajusta regressões quantílicas (por meio do pacote qgam) nos resíduos 
# e compara sua localização com a localização esperada
testQuantiles(simulationOutput)

#o gráfico calcula um teste de uniformidade por caixa e
# um teste de homogeneidade de variâncias entre caixas
par(mfrow=c(1,3))
plotResiduals(simulationOutput, form = soybean_data$Solo)
plotResiduals(simulationOutput, form = soybean_data$Ciclo4)
plotResiduals(simulationOutput, form = soybean_data$Caracteristica)

plot(simulationOutput)
# Reajusta o modelo com todos os otimizadores disponíveis
gm_all <- allFit(mod1.glmm)
ss <- summary(gm_all)


##### Mod GLMM 2 - Resp original interações duplas e Cultivar randon ####

mod2.glmm <- glmer(formula = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod2.glmm)
mod2.glmm@u

##### Mod GLMM 3 - Resp original interações duplas menos Solo e Caracteristica ####

mod3.glmm <- glmer(formula = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod3.glmm)
mod3.glmm@u
shapiro.test(mod3.glmm@u)
simulationOutput <- simulateResiduals(fittedModel = mod3.glmm)
testUniformity(simulationOutput)
plot(simulationOutput)
par(mfrow=c(1,3))
plotResiduals(simulationOutput, soybean_data$Solo)
plotResiduals(simulationOutput, soybean_data$Caracteristica)
plotResiduals(simulationOutput, soybean_data$Ciclo4)


##### Mod GLMM 4 - Resp original e covar Grupo e Cultivar randon ####

mod4.glmm <- glmer(formula = kgha ~ Grupo + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod4.glmm)
shapiro.test(mod4.glmm@u)
simulationOutput <- simulateResiduals(fittedModel = mod4.glmm)
testUniformity(simulationOutput)
plot(simulationOutput)


# testando os coeficientes não significantes do mod2.lme
coefID <- c(4,6,7,8,11,13,15,16)
coefNAM <- names(fixef(mod4.glmm)[coefID])
Cmatrix <- matrix(0,8,16)
Cmatrix[cbind(1:8,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod4.glmm, linfct = Cmatrix)
summary(GLH) # coeficientes não significativos


##### Mod GLMM 5 - Resp original e covar de Grupo ajustada com Cultivar randon ####

mod5.glmm <- glmer(formula = kgha ~ Grupo2 + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod5.glmm)
simulationOutput <- simulateResiduals(fittedModel = mod5.glmm)
testUniformity(simulationOutput)
plot(simulationOutput)
plotResiduals(simulationOutput, soybean_data$Grupo2)



##### Mod GLMM 7 - Resp kgha^0.47 link log interaç tripla e Cultivar randon ####

mod7.glmm <- glmer(formula = kgha^0.47 ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"))
summary(mod7.glmm)
shapiro.test(mod7.glmm@u)

# obtém os resíduos escalonados simulados
simulationOutput <- simulateResiduals(fittedModel = mod7.glmm, plot = F)
plot(simulationOutput)


##### Mod GLMM 8 - Resp kgha^0.47 link log covar Grupo e Cultivar randon ####

mod8.glmm <- glmer(formula = kgha^0.47 ~ Grupo + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"))
summary(mod8.glmm)
shapiro.test(mod8.glmm@u)
simulationOutput <- simulateResiduals(fittedModel = mod8.glmm)
testUniformity(simulationOutput)
plot(simulationOutput)


##### Mod GLMM 9 - Resp kgha^0.47 link log interaç tripla e Cultivar randon e var ####

mod9.glmm <- glmer(formula = kgha^0.47 ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"),
                   weights = varIdent(form = ~1|Cultivar))

mod <- lme(fixed = kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
           random = ~1|Cultivar, weights = varIdent(form = ~1|Cultivar) )




##### Mod GLMM 10 - Resp kgha^0.47 link log covar Grupo e Cultivar randon e var ####

mod10.glmm <- glmer(formula = kgha^0.47 ~ Grupo + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"),
                   weights = varIdent(form = ~1|Cultivar))


##### Mods GLMM que deram ERRO ####

# GLMM resp kgha family gaussian link inverse #### 
mod6.glmm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "inverse"))

# GLMM resp kgha family gaussian link link 1/mu^2 ##### 
mod6.glmm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "1/mu^2"))

# GLMM resp kgha family inverse.gaussian link 1/mu^2 ##### 
mod6.glmm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = inverse.gaussian(link = "1/mu^2"))

# GLMM resp kgha family inverse.gaussian link inverse #####
mod6.glmm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = inverse.gaussian(link = "inverse"))

# GLMM resp kgha family inverse.gaussian link log ####
mod6.glmm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = inverse.gaussian(link = "log"))

# GLMM resp kghaT family gaussian link log ####
mod6.glmm <- glmer(formula = kghaT ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"))
mod6.glmm@u

# GLMM resp log(kgha) family gaussian link log ####
mod6.glmm <- glmer(formula = log(kgha) ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"))
shapiro.test(mod6.glmm@u)

# GLMM resp kgha^0.47 family gaussian link log ####
mod6.glmm <- glmer(formula = kgha^0.47 ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                   data = soybean_data, family = gaussian(link = "log"))
shapiro.test(mod6.glmm@u)


#### mod GLMM - Resp kgha family Gamma link inverse #### 
mod20.glm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar), 
                   data = soybean_data, family = Gamma)
mod2.glmm@u

#### mod GLMM - Resp kgha family Gamma link 1/mu^2 #### 
mod20.glm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar), 
                   data = soybean_data, family = Gamma(link = "1/mu^2"))
mod2.glmm@u

#### mod GLMM - Resp kgha family Gamma link log #### 
mod20.glm <- glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar), 
                   data = soybean_data, family = Gamma(link = "log"))
mod2.glmm@u



##### LME from NLME #####

fit1.lme <- nlme::lme(fixed = kgha ~ Solo*Ciclo4*Caracteristica, 
                      data = soybean_data, random = ~1|Cultivar,
                      weights = varIdent(form = ~1|Cultivar))

summary(fit1.lme)
shapiro.test(fit1.lme$coefficients$random$Cultivar)
shapiro.test(rstandard(fit1.lme))


fit2.lme <- nlme::lme(fixed = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica + Solo*Caracteristica, 
                      data = soybean_data, random = ~1|Cultivar,
                      weights = varIdent(form = ~1|Cultivar))

summary(fit2.lme)
shapiro.test(fit2.lme$coefficients$random$Cultivar)
shapiro.test(fit2.lme$residuals)


fit3.lme <- nlme::lme(fixed = kgha ~ Grupo, 
                      data = soybean_data, random = ~1|Cultivar,
                      weights = varIdent(form = ~1|Cultivar))

summary(fit3.lme)
shapiro.test(fit3.lme$coefficients$random$Cultivar)
shapiro.test(fit3.lme$residuals)



fit4.lme <- nlme::lme(fixed = log(kgha) ~ Solo*Ciclo4*Caracteristica, 
                      data = soybean_data, random = ~1|Cultivar,
                      weights = varIdent(form = ~1|Cultivar))

summary(fit4.lme)
shapiro.test(fit4.lme$coefficients$random$Cultivar)
shapiro.test(fit4.lme$residuals)


fit5.lme <- nlme::lme(fixed = log(kgha) ~ Solo*Ciclo4 + Ciclo4*Caracteristica + Solo*Caracteristica, 
                      data = soybean_data, random = ~1|Cultivar,
                      weights = varIdent(form = ~1|Cultivar))

summary(fit5.lme)
shapiro.test(fit5.lme$coefficients$random$Cultivar)
shapiro.test(fit5.lme$residuals)


fit6.lme <- nlme::lme(fixed = log(kgha) ~ Grupo, 
                      data = soybean_data, random = ~1|Cultivar,
                      weights = varIdent(form = ~1|Cultivar))

summary(fit6.lme)
shapiro.test(fit6.lme$coefficients$random$Cultivar)
shapiro.test(fit6.lme$residuals)


# Outro Modelos testados --------------------------------------------------


# mod linear independente
mod1.lm <- lm(kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod1.lm)
plot(mod1.lm, which=2)
# Plintossolo apresenta maior variabilidade
boxplot(rstandard(mod1.lm) ~ soybean_data$Solo, xlab="Solo", ylab="Resíduos")
# há maior variabilidade nos Ciclos Médio e Tardio
boxplot(rstandard(mod1.lm) ~ soybean_data$Ciclo4, xlab="Ciclo", ylab="Resíduos")
# LaNina apresenta maior variabilidade que o Neutro
boxplot(rstandard(mod1.lm) ~ soybean_data$Caracteristica, xlab="Caracteristica", ylab="Resíduos")
# há uma variabilidade significativa nos diferentes genótipos
boxplot(rstandard(mod1.lm) ~ soybean_data$Cultivar, xlab="Cultivar", ylab="Resíduos")
# rejeita homocedasticidade
lmtest::bptest(mod1.lm)
# rejeita normalidade
tseries::jarque.bera.test(resid(mod1.lm))

bc <- MASS::boxcox(mod1.lm)
lambda <- bc$x[which.max(bc$y)]
soybean_data$kghaT <- (soybean_data$kgha^lambda - 1)/lambda

mod11.lm <- lm(kghaT ~ Solo*Ciclo4*Caracteristica, data = soybean_data)
summary(mod11.lm)
lmtest::bptest(mod11.lm)
tseries::jarque.bera.test(resid(mod11.lm))


# mod misto com interação tripla
mod1.lme <- lme(kgha ~ Solo*Ciclo4*Caracteristica, data = soybean_data, random = ~1|Cultivar)
summary(mod1.lme)
car::leveneTest(resid(mod1.lme) ~ soybean_data$Cultivar) # testa homocedasticidade
tseries::jarque.bera.test(resid(mod1.lme)) # testa normalidade

# mod misto com interações duplas
mod2.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod2.lme)
car::leveneTest(resid(mod2.lme) ~ soybean_data$Cultivar) # testa homocedasticidade
tseries::jarque.bera.test(resid(mod2.lme)) # testa normalidade

# mod misto sem interação Solo Caracteristica
mod3.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
# mod3 <- lmerTest::lmer(kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica + (1|Cultivar), soybean_data)
summary(mod3.lme)
car::leveneTest(resid(mod3.lme) ~ soybean_data$Cultivar) # testa homocedasticidade
tseries::jarque.bera.test(resid(mod3.lme)) # testa normalidade

# mod misto com a covar Grupo
# mod4 <- lmerTest::lmer(formula = kgha ~ Grupo + (1|Cultivar), data = soybean_data)
mod4.lme <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod4.lme)

# removendo os níveis não significativos G4, G8
# mod41 <- lmerTest::lmer(kgha ~ Grupo + (1|Cultivar), soybean_data,
#                         subset = !(Grupo %in% c("G4","G8","G11")))
mod41.lme <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar,
                 subset = !(Grupo %in% c("G4","G8","G11")))
summary(mod41.lme)

# mod com variável resposta em tonelada
# mod41 <- lmerTest::lmer(tha ~ Grupo + (1|Cultivar), soybean_data)
# summary(mod41)


# mod gls com interação tripla e variância no Cultivar
mod1.gls <- gls(model = kgha ~ Solo*Ciclo4*Caracteristica, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod1.gls)
boxplot(resid(mod1.gls, type = "normalized") ~ soybean_data$Cultivar, xlab="Cultivar", ylab="Resíduos")
tseries::jarque.bera.test(resid(mod1.gls))

# mod com variável resposta em tonelada
mod11.gls <- gls(model = kghaT ~ Solo*Ciclo4*Caracteristica, 
                 data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod11.gls)
boxplot(resid(mod11.gls, type = "normalized") ~ soybean_data$Cultivar, xlab="Cultivar", ylab="Resíduos")
car::leveneTest(resid(mod11.gls) ~ soybean_data$Cultivar) # rejeita homocedasticidade
tseries::jarque.bera.test(resid(mod11.gls)) # rejeita normalidade

# mod com variável resposta em tonelada e covar de Grupo
mod12.gls <- gls(model = kghaT ~ Grupo, data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod12.gls)
boxplot(resid(mod12.gls, type = "normalized") ~ soybean_data$Cultivar, xlab="Cultivar", ylab="Resíduos")
car::leveneTest(resid(mod12.gls) ~ soybean_data$Cultivar) # rejeita homocedasticidade
tseries::jarque.bera.test(resid(mod12.gls)) # rejeita normalidade

# mod gls com interações duplas e variância no Cultivar
mod2.gls <- gls(model = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica,
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod2.gls)
car::leveneTest(resid(mod2.gls) ~ soybean_data$Cultivar) # rejeita homocedasticidade
tseries::jarque.bera.test(resid(mod2.gls)) # rejeita normalidade
boxplot(resid(mod2.gls, type = "normalized") ~ soybean_data$Cultivar, xlab="Cultivar", ylab="Resíduos")

# estruturas de variância combinadas
vC.Sol.Cic.Car <- varComb(varIdent(form =~ 1|Solo), varIdent(form =~ 1|Ciclo4), varIdent(form =~ 1|Caracteristica))
vC.Cult.Car <- varComb(varIdent(form =~ 1|Cultivar), varIdent(form =~ 1|Caracteristica))

# mod gls com interação tripla e variância combinada no cultivar e caracteristica
mod3.gls <- gls(model = kgha ~ Solo*Ciclo4*Caracteristica, 
                data = soybean_data, weights = vC.Cult.Car)
summary(mod3.gls)
car::leveneTest(resid(mod3.gls) ~ soybean_data$Cultivar) # rejeita homocedasticidade
tseries::jarque.bera.test(resid(mod3.gls)) # rejeita normalidade
boxplot(resid(mod3.gls, type = "normalized") ~ soybean_data$Cultivar, xlab="Cultivar", ylab="Resíduos")
boxplot(resid(mod3.gls, type = "normalized") ~ soybean_data$Caracteristica, xlab="Caracteristica", ylab="Resíduos")

# mod gls com interação tripla e variância multiplicativa 
mod31.gls <- gls(model = kgha ~ Solo*Ciclo4*Caracteristica, 
                 data = soybean_data, weights = varIdent(form = ~1|Cultivar*Ciclo4))
summary(mod31.gls)

# mod gls com interações duplas e variância combinada no Cultivar e Caracteristica
mod4.gls <- gls(model = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica,
                data = soybean_data, weights = vC.Cult.Car)
summary(mod4.gls)


anova(mod1.gls, mod3.gls)
anova(mod2.gls, mod4.gls)

# mod gls sem a interação Solo Característica e variância no Cultivar
mod5.gls <- gls(model = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                data = soybean_data, weights = varIdent(form = ~1|Cultivar))
summary(mod5.gls)

