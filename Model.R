# Script para testes da modelagem do problema

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
bc <- MASS::boxcox(mod1.lm)
lambda <- bc$x[which.max(bc$y)]
soybean_data$kghaT <- (soybean_data$kgha^lambda - 1)/lambda


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
car::leveneTest(resid(mod1.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 2 - Resp original interações duplas e Cultivar de efeit aleat ####

mod2.lme <- lme(fixed = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod2.lme)
tseries::jarque.bera.test(resid(mod2.lme)) # rejeita normalidade
car::leveneTest(resid(mod2.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 3 - Resp original covar Grupo e Cultivar de efeit aleat ####

mod3.lme <- lme(fixed = kgha ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod3.lme)
tseries::jarque.bera.test(resid(mod3.lme)) # rejeita normalidade
car::leveneTest(resid(mod3.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 4 - Resp original covar Grupo2 e Cultivar de efeit aleat ####

mod4.lme <- lme(fixed = kgha ~ Grupo2, data = soybean_data, random = ~1|Cultivar)
summary(mod4.lme)
tseries::jarque.bera.test(resid(mod4.lme)) # rejeita normalidade
car::leveneTest(resid(mod4.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 5 - Resp transform por Box-Cox interação tripla e Cultivar de efeit aleat ####

mod5.lme <- lme(fixed = (kgha)^0.47 ~ Solo*Ciclo4*Caracteristica, data = soybean_data,
                random = ~1|Cultivar)
summary(mod5.lme)
tseries::jarque.bera.test(resid(mod5.lme)) # rejeita normalidade
shapiro.test(resid(mod5.lme))
car::leveneTest(resid(mod5.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 6 - Resp transform por Box-Cox interações duplas e Cultivar de efeit aleat ####

mod6.lme <- lme(fixed = (kgha)^0.47 ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod6.lme)
tseries::jarque.bera.test(resid(mod6.lme)) # rejeita normalidade
shapiro.test(resid(mod6.lme))
car::leveneTest(resid(mod6.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 7 - Resp transform por Box-Cox sem interações duplas de Solo e Característica ####

mod7.lme <- lme(fixed = (kgha)^0.47 ~ Solo*Ciclo4 + Ciclo4*Caracteristica, 
                data = soybean_data, random = ~1|Cultivar)
summary(mod7.lme)
tseries::jarque.bera.test(resid(mod7.lme)) # rejeita normalidade
shapiro.test(resid(mod7.lme))
car::leveneTest(resid(mod7.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 8 - Resp transform por Box-Cox covar Grupo e Cultivar de efeit aleat ####

mod8.lme <- lme(fixed = (kgha)^0.47 ~ Grupo, data = soybean_data, random = ~1|Cultivar)
summary(mod8.lme)
tseries::jarque.bera.test(resid(mod8.lme)) # rejeita normalidade
shapiro.test(resid(mod8.lme))
car::leveneTest(resid(mod8.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod MISTO 9 - Resp transform por Box-Cox covar Grupo2 e Cultivar de efeit aleat ####

mod9.lme <- lme(fixed = (kgha)^0.47 ~ Grupo2, data = soybean_data, random = ~1|Cultivar)
summary(mod9.lme)
tseries::jarque.bera.test(resid(mod9.lme)) # rejeita normalidade
shapiro.test(resid(mod9.lme))
car::leveneTest(resid(mod9.lme) ~ Cultivar, soybean_data) # rejeita homocedasticidade


##### Mod GLMM 1 - Resp original interaç tripla e Cultivar randon ####

mod1.glmm <- lme4::glmer(formula = kgha ~ Solo*Ciclo4*Caracteristica + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod1.glmm)


##### Mod GLMM 2 - Resp original interações duplas e Cultivar randon ####

mod2.glmm <- lme4::glmer(formula = kgha ~ Solo*Ciclo4 + Solo*Caracteristica + Ciclo4*Caracteristica + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod2.glmm)


##### Mod GLMM 3 - Resp original interações duplas menos Solo e Caracteristica ####

mod3.glmm <- lme4::glmer(formula = kgha ~ Solo*Ciclo4 + Ciclo4*Caracteristica + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod3.glmm)


##### Mod GLMM 4 - Resp original e covar Grupo e Cultivar randon ####

mod4.glmm <- lme4::glmer(formula = kgha ~ Grupo + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod4.glmm)
shapiro.test(resid(mod4.glmm))
# testando os coeficientes não significantes do mod2.lme
coefID <- c(4,6,7,8,11,13,15,16)
coefNAM <- names(fixef(mod4.glmm)[coefID])
Cmatrix <- matrix(0,8,16)
Cmatrix[cbind(1:8,coefID)] <- 1
rownames(Cmatrix) <- coefNAM
GLH <- multcomp::glht(model = mod4.glmm, linfct = Cmatrix)
summary(GLH) # coeficientes não significativos


##### Mod GLMM 5 - Resp original e covar de Grupo ajustada com Cultivar randon ####

mod5.glmm <- lme4::glmer(formula = kgha ~ Grupo2 + (1|Cultivar),
                         data = soybean_data, family = gaussian(link = "log"))
summary(mod5.glmm)
simulationOutput <- simulateResiduals(fittedModel = mod5.glmm)
testUniformity(simulationOutput)
plot(simulationOutput)



##### Mod GLS 1 - Resp original interaç tripla e Variância no Cultivar ####

# mod gls com interação tripla e variância no Cultivar
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
