# Testando a coleta de dados do NASA POWER



# Libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(sp)
library(nasapower)


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
  mutate(idCombi = as.factor(paste(Local, Ano, Ciclo, sep = "_")),
         ID = as.numeric(idCombi))

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
  mutate(Clima = as.factor(cut(Temp, breaks = c(-1.5,-1.0,-0.5,0.5), 
                               labels = c("ModerLaNina","FracoLaNina","Neutro"))))

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
                                          "G10", "G11", "G12", "G13", "G14", "G15", "G16")))



# data frame de informações para extração do NASA POWER
dados <- soybean_data |>
  select(ID, Ciclo, Latitude, Longitude, Plantio, Colheita) |>
  distinct(ID, .keep_all = TRUE)

dados <- arrange(dados, ID)

# NASA POWER RECOMENDADO --------------------------------------------------

library(devtools)
library(EnvRtype)

# Código de Exemplo pra extração dos dados do NASA POWER pelo pacote EnvRtype
env.data = get_weather(env.id = 'NAIROBI',country = 'KEN',
                       lat = -1.367,lon = 36.834,
                       start.day = '2015-03-01',end.day = '2015-04-01')

head(env.data)


# extração de dados do NASA POWER, com o country da erro
env.data <- get_weather(env.id = as.character(dados$id),
                        lat = dados$Latitude, lon = dados$Longitude, 
                        start.day = dados$Plantio, end.day = dados$Colheita)


# extração de dados do NASA POWER, sem o coutry fala que não é possível
env.data <- get_weather(env.id = as.character(dados$id),
                        lat = dados$Latitude, lon = dados$Longitude, 
                        start.day = dados$Plantio, end.day = dados$Colheita)


# NASA POWER API ----------------------------------------------------------


# Exemplo da extração dos dados do NASA POWER usando a API para o ID=48
daily_ag <- get_power(
  community = "ag",
  lonlat = c(-48.68183,-10.188028),
  pars = c("T2M", "T2MDEW", "T2M_MAX", "T2M_MIN", "WS2M", "RH2M", "TQV", "PRECTOTCORR"),
  dates = c("2022-11-17", "2023-02-25"),
  temporal_api = "daily"
)
daily_ag

# Exemplo da extração dos dados do NASA POWER usando a API para o ID=55
daily_ag2 <- get_power(
  community = "ag",
  lonlat = c(-48.68183,-10.188028),
  pars = c("T2M", "T2MDEW", "T2M_MAX", "T2M_MIN", "WS2M", "RH2M", "TQV", "PRECTOTCORR"),
  dates = c("2022-11-17", "2023-03-07"),
  temporal_api = "daily"
)
daily_ag2


# Comando pra ver qual combinação de parâmetros é valida para a função 
purrr::imap(nasapower:::parameters, function(x, n) any(x=="PRECTOTCORR"))


# variáveis coletadas
# ALLSKY_SFC_LW_DWN --> Fluxo radiativo infravermelho térmico descendente (onda longa)
# 
# ALLSKY_SFC_SW_DWN --> Incidente de insolação total do céu em uma superfície horizontal
#
# PRECTOTCORR --> Precipitação corrigida 
# A média corrigida pela polarização da precipitação total na superfície da terra
# em massa de água (inclui o conteúdo de água na neve)
# 
# RH2M --> Umidade relativa a 2 metros
# 
# T2M --> Temperatura a 2 metros
# 
# T2MDEW --> Ponto de orvalho/gelo a 2 metros
# 
# T2M_MAX --> Temperatura máxima a 2 metros
# 
# T2M_MIN --> Temperatura Mínima a 2 Metros
# 
# TQV --> Água precipitável da coluna total
# 
# WS2M --> Velocidade do vento a 2 metros

get_nasap_data <- function(id){
  
  datas <- c(dados$Plantio[id], dados$Colheita[id])
  longlat <- c(dados$Longitude[id],dados$Latitude[id])
  
  # comando que extraí os dados da API 
  daily_ag <- get_power(community = "ag", temporal_api = "daily",
                        pars = c("ALLSKY_SFC_LW_DWN", "ALLSKY_SFC_SW_DWN",
                                 "PRECTOTCORR", "RH2M", "T2M", "T2MDEW",
                                 "T2M_MAX", "T2M_MIN", "TQV","WS2M"),
                        lonlat = longlat,
                        dates =  datas)
  
  nr <- dados$Ciclo[which(dados$ID == id)]+1
  daily_ag$ID <- rep(id, nr)
  
  
  # resumindo a informação do período pra cada id
  dados_resumidos <- daily_ag |> group_by(ID) |> 
    summarise_all(.funs = list(min, max, mean))
  dados_resumidos
  
}


# aplicando a função que coleta dos dados da API no vetor de ids
nasaPowerData <- map_df(dados$ID, get_nasap_data)


# renomeando as colunas pelo nome da função usada
colnames(nasaPowerData) <- gsub(pattern = '\\_fn1', replacement = '\\_MIN', x = names(nasaPowerData))
colnames(nasaPowerData) <- gsub(pattern = '\\_fn2', replacement = '\\_MAX', x = names(nasaPowerData))
colnames(nasaPowerData) <- gsub(pattern = '\\_fn3', replacement = '\\_MEAN', x = names(nasaPowerData))


# Selecionando as variáveis de interesse
new_vars <- nasaPowerData |>
  select(ID, ALLSKY_SFC_LW_DWN_MIN:WS2M_MIN,
         ALLSKY_SFC_LW_DWN_MAX:WS2M_MAX,
         ALLSKY_SFC_LW_DWN_MEAN:WS2M_MEAN)


# fazendo a junção do conjunto de dados original com os dados obtidos da API
data_join <- left_join(x = soybean_data, y = new_vars, by = "ID")

# escrevendo o conjunto de dados final
write.csv(x = data_join, file = "data_join.csv")

