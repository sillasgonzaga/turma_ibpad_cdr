#### Leitura e importação de dados ----

#0## Baixe o arquivo do link abaixo e salve na pasta onde vc criou o projeto no rstudio na aula 1.
# use a função read_csv() para importar o arquivo. Crie o objeto "df" para salvar o resultado...
# do read_csv()

# http://data.insideairbnb.com/brazil/rj/rio-de-janeiro/2020-03-18/data/listings.csv.gz
library(tidyverse)
df <- read_csv("listings.csv.gz")
df

#1#  Crie uma coluna indicando o porcentual de dias que o anúncio está disponível..
# para locação nos próximos 30 dias
glimpse(df)
df$percentual_disponivel_30 <- df$availability_30/30

# sintaxe do mutate:
# mutate(dataframe, coluna = f(...))
# dataframe %>% mutate(coluna = f(...))
df <- df %>% mutate(percentual_disponivel_30 = availability_30/30)

#2# Analise a distribuição da variável review_scores_rating com summary() e quantile().
# O que você pode aprender com os resultados?
df$review_scores_rating
summary(df$review_scores_rating)
quantile(df$review_scores_rating, na.rm = TRUE)
?quantile

#3# Faça o mesmo acima para analisar a distribuição da variável price.
df$price
summary(df$price)

#4# Retorne a quantidade de bairros distintos (neighbourhood) que estão presentes..
# nos anúncios com unique() e length()
df$neighbourhood
unique(df$neighbourhood)
length(unique(df$neighbourhood))
# sintaxe do pipe %>% (ctrl shift m)
df$neighbourhood %>% unique() %>% length()

# refazer a questao usando funçoes do pacote dplyr
df %>% distinct(neighbourhood)


#5# Retorne quantos anúncios estão localizados no bairro do Flamengo, tanto..
# em quantidade absoluta como em porcentual

# sintaxe da funcao nrow(): nrow(dataframe)
nrow(df)

# sintaxe dplyr
qtd_anuncios_flamengo <- df %>% filter(neighbourhood == "Flamengo") %>% nrow()
# quantidade relativa
qtd_anuncios_flamengo/nrow(df)

944 / 36085
'f' == 'F'

# sintaxe r base
# quantidade absoluta
sum(df$neighbourhood == 'Flamengo', na.rm = TRUE)
# quantidade relativa
nrow(df) - sum(is.na(df$neighbourhood)) # denominador sem NA
sum(!is.na(df$neighbourhood))
mean(df$neighbourhood == 'Flamengo', na.rm = TRUE)

#6# Calcule quantos imóveis não possuem informação da metragem do quarto/imóvel...
# (coluna square_feet). Dica: A função is.na() retorna TRUE se o elemento for... 
# NA e FALSE se não for.

# sintaxe r base sem pipe
mean(is.na(df$square_feet))
# sintaxe r base com pipe
df$square_feet %>% is.na() %>% mean()
# sintaxe dplyr
df %>% filter(is.na(square_feet)) %>% nrow()
# sintaxe dplyr usando mutate e summarise
df %>% 
  select(neighbourhood, square_feet) %>% 
  mutate(tem_dado_de_m2 = is.na(square_feet)) %>% 
  # sintaxe summarise(): dataframe %>% summarise(nome_nova_coluna = funcao_agregacao(...))
  group_by(neighbourhood) %>% 
  summarise(porcentual_nas = mean(tem_dado_de_m2))
  
#7# Qual o número máximo de anúncios por host? (coluna host_total_listings_count)
max(df$host_total_listings_count, na.rm = TRUE)

glimpse(df)

# quem é a pessoa que tem 473 anuncios no airbnb
df %>% 
  filter(host_total_listings_count == max(host_total_listings_count, na.rm = TRUE)) %>% 
  select(host_url)

# usando group_by por bairro
df %>% 
  group_by(neighbourhood) %>% 
  filter(host_total_listings_count == max(host_total_listings_count, na.rm = TRUE)) %>% 
  select(host_url, host_total_listings_count)

##

#### Manipulação ou manuseio de dados (data wrangling) -------------------------

#0# converter as colunas de preço para numéricas
# dica: use a funçao parse_number() para converter um texto em numerico

# exemplo de uso de parse_number
parse_number("R$1000")
df_limpo <- df %>%  mutate(preco_correto = parse_number(price))

#1# Retorne a url (listing_url) do anúncio mais caro do airbnb
df_limpo %>% 
  select(listing_url, preco_correto) %>% 
  arrange(desc(preco_correto))

#2# Retorne o nome do host (host_name) que tem a maior quantidade de anúncios
df_limpo %>% 
  arrange(desc(host_total_listings_count)) %>% 
  select(host_name, host_total_listings_count, host_listings_count)

#3# Retorne a quantidade de hosts por ano em que entrou no airbnb
# Dica: para extrair o ano de uma date, use a função year() do lubridate
library(lubridate)

## resposta errada:

df_limpo %>% 
  select(host_since) %>% 
  mutate(ano = year(host_since)) %>% 
  group_by(ano) %>% 
  summarise(qtd_anuncios = n())


df_limpo %>% 
  select(host_since) %>% 
  mutate(ano = year(host_since)) %>% 
  count(ano)

## resposta certa:
df_limpo %>% 
  arrange(desc(host_total_listings_count)) %>% 
  select(host_id, host_name, host_since) %>% 
  distinct(host_id, .keep_all = TRUE) %>% 
  mutate(ano = year(host_since)) %>% 
  count(ano)



#4# Selecione as colunas name e space e filtre as linhas que contem a palavra..
# praia em space. Dica: Vc pode usar a função str_detect() dentro de filter() ou...
# de mutate()
str_detect(df_limpo$space, "praia")
df_limpo$space[1]
df_limpo$space[8]
df_limpo$space[8] %>% str_detect("praia")

# usando filter()
df_limpo %>% 
  select(name, space) %>% 
  filter(str_detect(space, 'praia'))

# usando mutate()
df_limpo %>% 
  select(name, space) %>% 
  mutate(tem_praia = str_detect(space, 'praia')) %>% 
  filter(tem_praia)

# usando filter() com condição OU 
df_limpo %>% 
  select(name, space) %>% 
  filter(str_detect(space, 'praia') | str_detect(space, 'beach'))


#5# Imóveis que mencionam a palavra praia são em média mais caros?
df_limpo %>% 
  select(name, space, preco_correto) %>% 
  mutate(tem_praia = str_detect(space, 'praia') | str_detect(space, 'beach')) %>% 
  group_by(tem_praia) %>% 
  summarise(media_preco = median(preco_correto))

summary(df_limpo$preco_correto)


#6# Use mutate() para modificar o dataframe criando uma coluna booleana chamada...
# `esgotado` informando se o imovel esta indisponivel para os proximos 30 dias...
# (coluna availability_30)
df_limpo <- df_limpo %>% 
  mutate(esgotado = availability_30 == 0)

glimpse(df_limpo)
#6# Quais os 5 bairros que possuem mais de 100 anúncios com a maior taxa de...
# anúncios esgotados nos próximos 30 dias?
# Dica: crie duas colunas com summarise, uma usando n() e outra com mean(esgotado)...
# e depois use filter(), arrange() e head()
df_limpo %>% 
  group_by(neighbourhood) %>% 
  summarise(qtd_anuncios = n(),
            taxa_media_ocupacao = mean(esgotado)) %>% 
  filter(qtd_anuncios > 100) %>% 
  arrange(desc(taxa_media_ocupacao)) %>% 
  head(5)


#7# Retorne a quantidade de anúncios e reviews (number_of_reviews) por bairro,#
# calcule uma taxa de...
# quantidade de reviews por quantidade de anuncios. Os bairros que possuem...
# mais anuncios são, proporcionalmente, os que tem mais reviews?
df_limpo$number_of_reviews

df_limpo %>% 
  group_by(neighbourhood) %>% 
  summarise(qtd_anuncios = n(),
            qtd_reviews = sum(number_of_reviews)) %>% 
  mutate(taxa_review_por_anuncion = qtd_reviews/qtd_anuncios) %>% 
  arrange(desc(qtd_anuncios))


#8# Quais são os diferentes tipos de anúncio (quarto, apt, etc.) que existem?
# Coluna room_type
df_limpo %>% 
  distinct(room_type)


#9# A quantidade de quartos tem relação com o preço dos apartamentos inteiros? 
glimpse(df_limpo)

df_limpo %>% 
  filter(room_type == "Entire home/apt") %>% 
  group_by(bedrooms) %>% 
  summarise(qtd_anuncios = n(),
            preco_mediano = median(preco_correto))


#10# DESAFIO
# Suponha que você planeja uma viagem para o RJ com mais 1 pessoa de 5 diárias nos...
# proximos 30 dias. Você  e seu grupo têm alguns critérios de escolha: 
# - Vocês querem ficar em Ipanema, Copacabana ou Leblon.
# - Vocês preferem que o host esteja no mesmo bairro.  
# - Não desejam pagar um depósito de segurança;
# - Querem um apartamento inteiro só para vocês que seja "instant bookable"
# - A diária já inclua duas pessoas

# Filtre os anúncios que atendem aos critérios acima e crie uma coluna
# chamada preco_total_viagem,
# com a formula sendo:  taxa de limpeza + preço da diaria x quantidade de diarias.
# Compare os resultados com os do site.
# Dica: Comece com o código abaixo, selecionando as colunas importantes 


df_limpo %>% 
  select(listing_url, neighbourhood, host_neighbourhood, 
         availability_30, minimum_nights, security_deposit,
         instant_bookable, guests_included, preco_correto, room_type, 
         cleaning_fee) %>%
  #glimpse() %>% 
  mutate(security_deposit = parse_number(security_deposit),
         cleaning_fee = parse_number(cleaning_fee)) %>% 
  # substuir NAs em cleaning_fee por 0
  mutate(cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee)) %>% 
  filter(neighbourhood %in% c("Copacabana", "Leblon", 'Ipanema'),
         neighbourhood ==  host_neighbourhood,
         security_deposit == 0,
         instant_bookable,
         guests_included >= 2) %>% 
  mutate(preco_total_viagem = cleaning_fee + preco_correto * 5) %>% 
  #filter(is.na(preco_total_viagem)) %>% 
  #glimpse()
  select(listing_url, neighbourhood, preco_total_viagem) %>% 
  group_by(neighbourhood) %>% 
  summarise(preco_medio_esperado = median(preco_total_viagem))


"Copa" %in% c("Copacabaca", "Leblon", 'Ipanema')
"Copa" == "Copacabana"
