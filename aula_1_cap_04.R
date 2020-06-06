inteiro <- 928L
outro.inteiro <- 5e2
decimal <- 182.93
caracter <- 'exportação'
logico <- TRUE
outro.logico <- FALSE

class(inteiro)
class(decimal)
class(outro.inteiro)

class(caracter)


class(123)
class("123")

a1 = "2"
a2 = "3"
a1 + a2
'a' + "b"

class(senado)


as.numeric("a")

as.data.frame(senado)
senado

# construindo seu primeiro vetor (conjunto de numeros/character/datas/logical)
y <- 9
vetor_num  = c(1, 3, 5, 7, y)

vetor_num_2 = vetor_num * 2

mean(vetor_num)
sum(vetor_num)



# vetor de caracteres
vetor.chr <- c('tipo1', 'tipo2', 'tipo3', 'tipo4')
vetor.chr
class(FALSE)
class(TRUE)

vetor_misturado <- c(1, 2, "c", 4)
vetor_misturado
class(vetor_misturado)

vetor_bool <- c(TRUE, TRUE, FALSE, TRUE)
class(vetor_bool)

mean(vetor_misturado)
sum(vetor_bool)
mean(vetor_bool)

v1 <- c(1, 3, 5)
# acrescentar 7 ao vetor v1
c(0, v1)

# -------
data("airquality") # carrega uma base de dados pré-carregada no R
head(airquality)
?airquality

airquality$Solar.R <- airquality$Solar.R * 10
mean(airquality$Solar.R)

head(airquality)
airquality$Solar.R <- airquality$Solar.R / 10

airquality$ano <- 1973
head(airquality)

?mean
mean(airquality$Solar.R)
# media desconsiderando NAs
mean(airquality$Solar.R, na.rm = TRUE)
summary(airquality$Solar.R)
sum(is.na(airquality$Solar.R))
# inverter o is.na()
length(airquality$Solar.R) - sum(is.na(airquality$Solar.R))

sum(!is.na(airquality$Solar.R))

# bind_rows(): concatenar dois ou mais dataframes
?bind_rows

tail(airquality)

novo_df <- data.frame(Ozone = c(18, 10), 
                      Solar.R = c(200, 180),
                      Wind = c(NA, NA),
                      Temp = c(70, 70),
                      Month = c(10, 10),
                      Day = c(1, 2),
                      ano = c(1973, 1973))

airquality2 <- bind_rows(airquality, novo_df)
tail(airquality2)

# exemplos com IF
# ==
x1 <- 10

print("Hoje é sábado.")
x2 <- c(1, 2, 3)

if (x2 == 1){
  print('O input é igual a 2')
} else{
  #x1 + 20
  print("O input não é igual a 2")
}

ifelse(x2 == 1, "== 1", '!+ 1')


for(i in x2){
  print(i)
  # fazer teste logico if dentro do for loop
  if(i == 1){
    print('Igual a 1')
  } else {
    print('Diferete de 1')
  }
  
}

4 / 2
5 / 2

4 %% 2 == 0
5 %% 2 == 0


fah_para_celsius <- function(x){
  x2 <- (x - 32) * 5/9
  x2 <- round(x2, 2)
  return(x2)
}

### carregar funcoes que estao em outro script.R
source("minhas_funcoes.R")



fah_para_celsius(60)


airquality$temp_c <- fah_para_celsius(airquality$Temp)

head(airquality)

### outras funcoes uteis para vetores
senado$Party
unique(senado$Party)

vetor <- seq(from=0, to=100, by=15) #vetor de 0 a 100, de 15 em 15.
vetor

vetor[1]
vetor[15]

vetor[3] <- 31
vetor


senado[1:5, 1:3]

1:5
# é equivalente a 
seq(from = 1, to = 5, by = 1)

sum(sqrt(x1))
# reescrever o comando acima com pipe
# ctrl shift m
x1 %>% sqrt() %>% sum()

# funcao select()
?select
select(senado, SenatorUpper, Party)

select(senado, -SenNumber)

senado %>% select(SenatorUpper, Party)

# selecionar colunas que começam com S
?starts_with
senado %>% select_at(vars(starts_with('S')))

# select_if()
is.numeric(senado$VoteNumber)
is.numeric(senado$SenNumber)
senado %>% select_if(is.numeric)
# select_all()
?select_all
senado %>% select_all(str_to_lower)

### renomear colunas
# sintaxe rename: novo nome = nome antigo
senado %>% rename(nome_senador = SenatorUpper)
# renomear usando select
senado %>% select(nome_senador = SenatorUpper)


##### filter
filter(senado, Party == "PT")
senado %>% filter(Party == "PT")
senado %>% filter(Party != "PT")

senado %>% filter(Party == "PT" | State == "SP") %>% tail()

capitais
str_sub(capitais, 1, 1)
# filtrar senadores que começam com S
senado %>% filter(str_sub(SenatorUpper, 1, 1) == "S")

# criar funcao para pegar a primeira letra de uma string
primeira_letra <- function(x){
  str_sub(x, 1, 1)
}

primeira_letra(capitais)

senado %>% filter(primeira_letra(SenatorUpper) == "S")


senado %>% filter(primeira_letra(SenatorUpper) == "S") %>% select(SenatorUpper)
senado %>% filter(primeira_letra(SenatorUpper) == "S") %>% distinct(SenatorUpper)

senado %>% filter(primeira_letra(SenatorUpper) == "S") %>% pull(SenatorUpper) %>% unique()

airquality$Month
pull(airquality, Month)
airquality %>% pull(Month)


senado %>% 
  select(VoteNumber, SenatorUpper) %>% 
  # sintaxe de mutate: mutate(coluna = f(...))
  mutate(senador_minusculo  = tolower(SenatorUpper),
         senador_nome = str_to_title(SenatorUpper
                                     ))

senado <- senado %>% mutate(senador_nome = str_to_title(SenatorUpper))
senado

senado %>% select(VoteNumber, senador_nome, PercentYes) %>% distinct(VoteNumber, PercentYes)

senado %>% mutate(Vote = ifelse(Vote == "S", 'Sim', 'Não'))

senado
senado <- senado %>% 
  mutate(ano = str_sub(VoteNumber, start = 1, end = 4))


senado %>% select(ano, VoteNumber)
senado %>% distinct(ano)

senado %>% 
  group_by(ano) %>% 
  summarise(qtd_votos = n())


# por ano, saber:
# quantidade de votos sim
senado %>% 
  filter(Vote == "S") %>% 
  group_by(ano) %>% 
  summarise(qtd_votos = n())

# percentual de votos sim
senado %>% 
  select(ano, Vote) %>% 
  mutate(voto_sim = ifelse(Vote == 'S', 1, 0)) %>% 
  group_by(ano) %>% 
  summarise(pct_voto_sim = mean(voto_sim))



## arrange()
senado %>% 
  select(ano, Vote) %>% 
  mutate(voto_sim = ifelse(Vote == 'S', 1, 0)) %>% 
  group_by(ano) %>% 
  summarise(pct_voto_sim = mean(voto_sim)) %>% 
  arrange(desc(pct_voto_sim))



senado %>% 
  select(ano, voto_sim) %>% 
  mutate(voto_sim = ifelse(Vote == 'S', 1, 0))  
  


