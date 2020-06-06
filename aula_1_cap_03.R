library(tidyverse)

senado <- read_csv("senado.txt")
senado <- read_delim("senado.txt", delim = ",")

senado


# funcoes para iniciar a analise
head(senado, 15)

tail(senado)

str(senado)
glimpse(senado)


?read_csv()
