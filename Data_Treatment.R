

# Libraries ---------------------------------------------------------------

library(xlsx)
library(tidyverse)



# Data Treatment ----------------------------------------------------------

# loading principal data file
main_data <- read.xlsx("Data/Dados - Produtividade em Latossolo e Plintossolo - 2018 a 2023.xlsx", sheetIndex = 1)
main_data <- main_data[,1:12]

soybean_data <- main_data |>
  # removendo acentos
  mutate_at(vars(Local, Textura.do.solo, Cultivar), ~stringi::stri_trans_general(.,"Latin-ASCII")) |>
  # transformando variáveis categóricas
  mutate_at(vars(Local,Ano,Solo,Cultivar, Textura.do.solo), as.factor) |>
  # transformando as covariáveis de Data
  mutate_at(vars(Plantio), as.Date) |>
  mutate(Colheita = Plantio+Ciclo) |>
  separate(Lat..e.Long., into = c("Latit", "Longit"), sep = ";")

# correcting coordinates
soybean_data <- soybean_data %>%
  mutate(Latit = case_when(
    Local == "Pium" ~ "10°12' 58'' S",
    Local == "Paraiso do Tocantins" ~ "10°11' 16.9'' S",
    TRUE ~ as.character(Latit)
  ),
  Longit = case_when(
    Local == "Pium" ~ "49°15' 1.4'' W",
    Local == "Paraiso do Tocantins" ~ "48°40' 54.6'' W",
    TRUE ~ as.character(Longit)
  ))



  mutate(Latitude = char2dms(from = Latit,chd = "°", chm = "'", chs = "''") 
         |> as.numeric()) |>
  mutate(Longitude = char2dms(from = Longit,chd = "°", chm = "'", chs = "''")
         |> as.numeric()) |>
  mutate(Longitude = -Longitude)


  # ajustando os níveis das variáveis categóricas
  mutate(Ciclo4 = factor(Ciclo4, levels = c("Super Precoce","Precoce","Medio","Tardio")))
