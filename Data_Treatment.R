

# Libraries ---------------------------------------------------------------

library(xlsx)
library(tidyverse)



# Data Treatment ----------------------------------------------------------


main_data <- read.xlsx("Data/Dados - Produtividade em Latossolo e Plintossolo - 2018 a 2023.xlsx", sheetIndex = 1)
main_data <- main_data[,1:12]

