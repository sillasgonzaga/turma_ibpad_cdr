#### capitulo 8 ----

x <- c("2014-07-15", "2018/03/20", "2019-12-31", "20170511")

x
class(x)

class("2020-05-03")
class(as.Date("2020-05-03"))

as.Date("2020-05-03") + 1

x
as.Date(x)

library(lubridate)

x
as_date(x)


# gerar vetor de datas separadas por mes
seq.Date(from = as_date("2020-01-01"),
         to = as_date("2020-12-01"),
         by = "7 days")

# data de hoje
today()

seq.Date(from = today(),
         by = '1 month',
         length.out = 12)


# observer como usamos diferentes separadores
datas_brasil <- c("01/12/2019", "20/11/2018",
                  "30011990", "17-03-2000")

datas_brasil
as.Date(datas_brasil)

as_date(datas_brasil)

datas_brasil_str <- datas_brasil %>% str_remove_all("/|-")

ano <- str_sub(datas_brasil_str, -4)
mes <- str_sub(datas_brasil_str, 3, 4)
dia <- str_sub(datas_brasil_str, 1, 2)

mes
dia
ano

paste()
paste0()

as.Date(str_c(ano, mes, dia, sep = "/"))
class(as.Date(str_c(ano, mes, dia, sep = "/")))

paste(ano, mes, dia, sep = "/")

datas_brasil + 10
dmy(datas_brasil) + 10


datas_brasil <- dmy_hms(c("01/12/2019 13:51:15",
                          "20/11/2018 00:00:00",
                          "30011990 080000",
                          "17-03-2000 203000"))
datas_brasil

day(datas_brasil)
month(datas_brasil)
year(datas_brasil)
# retornar o nome do mês
month(datas_brasil, label = TRUE, abbr = FALSE)

hour(datas_brasil)
wday(datas_brasil)
week(datas_brasil)

class(datas_brasil)

datas_brasil + ddays(1)

datas_brasil + dweeks(2)


data1 <- dmy_hms("01/09/1993 20:00:00")
data2 <- dmy_hms("24-06-2018 17:00:00")

data1
data2

class(data2 - data1)
?difftime

data2 - data1

difftime(data2, data1, units = 'days')
as.numeric(difftime(data2, data1, units = 'days'))/30

datas_brasil

floor_date(datas_brasil, "month")
floor_date(datas_brasil, "week")
floor_date(datas_brasil, "year")
floor_date(datas_brasil, "quarter")

datas_brasil
ceiling_date(datas_brasil, "month") - ddays(1)

#### capitulo 9 ----

df_cpf <- tibble(
  cpf = c("15274540023", "47928968002", "08293701005"),
  nome = c("A", "B", "C")
)

# exportar dataframe no computador
write_csv(df_cpf, "dados/cpfs.csv")

df_cpf_2 <- read_csv("dados/cpfs.csv")

# salvando em rds
write_rds(df_cpf, "dados/cpfs.rds")
read_rds("dados/cpfs.rds")

ano
# salvar um vetor no computador
write_rds(ano, "dados/vetor_de_anos.rds")

read_rds("dados/vetor_de_anos.rds")

# exportar dataframe em excel: writexl
# ler arquivos em excel: readxl


#### capitulo 11 ----
a <- c(1, 2, "c", 4)
a

list(1, 2, "c", 4)

obj_data_frame <- head(iris)
obj_elemento_unico_inteiro <- 1
obj_um_na <- NA
obj_vetor_string <- letters[1:5]
obj_modelo_regressao <- lm(mpg ~ wt, data = mtcars)

obj_data_frame
obj_elemento_unico_inteiro
obj_um_na
obj_vetor_string
obj_modelo_regressao


minha_lista <- list(obj_data_frame, 
     obj_elemento_unico_inteiro,
     obj_um_na,
     obj_vetor_string,
     obj_modelo_regressao)


minha_lista

# extrair um elemento de uma lista
minha_lista[4]

data_frame <- head(iris)
elemento_unico_inteiro <- 1
um_na <- NA
vetor_string <- letters[1:5]
modelo_regressao <- lm(mpg ~ wt, data = mtcars)

minha_lista <- list(meu_data_frame = data_frame, 
                    numero_um = elemento_unico_inteiro, 
                    # este elemento abaixo não vai possuir um nome
                    um_na, 
                    letras = vetor_string,
                    modelo = modelo_regressao)

minha_lista$letras
minha_lista$um_na
# função para extrair nomes dos elementos de uma lista
names(minha_lista)[3] <- "meu_na"

minha_lista$meu_na

c(1, 2, 3) * 2

meu_vetor <- c(1, -3, 5, -10)

meu_vetor
# valor absoluto
abs(meu_vetor)

minha_lista <- list(1, -3, 5, -10)

abs(minha_lista)

library(purrr)
# sintaxe de map: map(objeto, funcao a aplicar iterativamente)
minha_lista
map(minha_lista, abs)

?map


lista_com_decimais <- list(1.4, c(1.1, 1.333333, 4.666666), NA)

# isso
map(lista_com_decimais, round, digits = 3)
# é equivalente a isto
round(lista_com_decimais[[1]], 3)
round(lista_com_decimais[[2]], 3)
round(lista_com_decimais[[3]], 3)




list(minha_lista, lista_com_decimais)


l <- list(v1 = c(1, 3, 5), v2 = c(2, 4, 6), v3 = c(7, 8, 9))

# sintaxe 1
map(l, sum)

minha_soma <- function(x) {
  sum(x)
}

# sintaxe 2
map(l, function(y) sum(y))
map(l, function(a) a * 2)

# sintaxe 3 
l
map(l, ~ sum(.x))
map(l, ~ .x * 2)


map_dbl(l, sum)



#### projeto capitulo 11
dir("projeto_com_listas/")

arquivo <- "projeto_com_listas/COMED_hourly.csv"

df_exemplo <- read_csv("projeto_com_listas/COMED_hourly.csv")

nome_regiao <- colnames(df_exemplo)[2]
nome_regiao

library(lubridate)
df_exemplo %>% 
  rename(consumo = 2) %>% 
  mutate(regiao = nome_regiao,
         mes = month(Datetime)) %>% 
  group_by(regiao, mes) %>% 
  summarise(consumo_medio = mean(consumo))


calcular_media_mensal_por_regiao <- function(nome_arquivo){
  # importar o arquivo para um dataframe
  df <- read_csv(nome_arquivo)
  # extrair nome da regiao (nome da segunda coluna)
  nome_regiao <- colnames(df)[2]
  
  # criar dataframe de output
  df %>% 
    rename(consumo = 2) %>% 
    mutate(regiao = nome_regiao,
           mes = month(Datetime)) %>% 
    group_by(regiao, mes) %>% 
    summarise(consumo_medio = mean(consumo))
}

calcular_media_mensal_por_regiao("projeto_com_listas/AEP_hourly.csv")
calcular_media_mensal_por_regiao("projeto_com_listas/DAYTON_hourly.csv")


# criar vetor com nomes dos arquivos csv
arquivos <- dir("projeto_com_listas/", pattern = "_hourly.csv$",
                full.names = TRUE)
arquivos

lista_dfs_agregagos <- map(arquivos, calcular_media_mensal_por_regiao)

dfs_agregagos_geral <- map_df(arquivos, calcular_media_mensal_por_regiao)

dfs_agregagos_geral
bind_rows(lista_dfs_agregagos)





