#### Introdução ao R e Rstudio --------------

#1# Crie dois objetos, x1 e x2, composto pelos cinco primeiros números ímpares
# e pares, respectivamente. DIca: função seq().
x1 = seq(1, length.out = 5, by = 2) # alt shift seta baixo
x2 = seq(0, length.out = 5, by = 2) 
x1
x2

#2# Calcule a soma dos elementos de x1
sum(x1)

#3# Calcule a soma dos elementos de x2 usando o pipe
sum(x2)

#4# Some o vetor x1 com x2, calculando a soma elemento a elemento 
x1 + x2

#5# Crie um vetor chamado capitais com os nomes das capitais do Sudeste.
# Use a função str_length() para retornar o tamanho de cada elemento do vetor
library(tidyverse)
capitais <- c("São Paulo", 'Rio de Janeiro', "Vitória", 'Belo Horizonte', NA)
capitais
str_length(capitais)
nchar(capitais)
length(capitais)
?str_length

#6# Eleve o vetor x2 ao quadrado com ^2
x2 ^ 2



#### Leitura e importação de dados ----
#1#  Crie uma coluna indicando o porcentual de dias que o anúncio está disponível..
# para locação nos próximos 30 dias

#2# Analise a distribuição da variável review_scores_rating com summary() e quantile().
# O que você pode aprender com os resultados?

#3# Faça o mesmo acima para analisar a distribuição da variável price.

#4# Retorne a quantidade de bairros distintos (neighbourhood) que estão presentes..
# nos anúncios com unique() e length()

#5# Retorne quantos anúncios estão localizados no bairro do Flamengo, tanto..
# em quantidade absoluta como em porcentual

#6# Calcule quantos imóveis não possuem informação da metragem do quarto/imóvel...
# (coluna square_feet). Dica: A função is.na() retorna TRUE se o elemento for... 
# NA e FALSE se não for.

#7# Qual o número máximo de anúncios por host? (coluna host_total_listings_count)

##

#### Manipulação ou manuseio de dados (data wrangling) -------------------------


#1# Retorne a url (scrape_url) do anúncio mais caro do airbnb

#2# Retorne o nome do host (host_name) que tem a maior quantidade de anúncios

#3# Retorne a quantidade de hosts por ano em que entrou no airbnb

#4# Selecione as colunas name e space e filtre as linhas que contem a palavra..
# praia em space. Dica: Vc pode usar a função str_detect() dentro de filter() ou...
# de mutate()

#5# Imóveis que mencionam a palavra praia são em média mais caros?

#6# Use mutate() para modificar o dataframe criando uma coluna booleana chamada...
# `esgotado` informando se o imovel esta indisponivel para os proximos 30 dias...
# (coluna availability_30)

#6# Quais os 5 bairros que possuem mais de 100 anúncios com a maior taxa de...
# anúncios esgotados nos próximos 30 dias?
# Dica: crie duas colunas com summarise, uma usando n() e outra com mean(esgotado)...
# e depois use filter(), arrange() e head()

#7# Retorne a quantidade de anúncios e reviews (number_of_reviews) por bairro,#
# calcule uma taxa de...
# quantidade de reviews por quantidade de anuncios. Os bairros que possuem...
# mais anuncios são, proporcionalmente, os que tem mais reviews?

#8# Quais são os diferentes tipos de anúncio (quarto, apt, etc.) que existem?
# Coluna room_type

#9# A quantidade de quartos tem relação com o preço dos apartamentos inteiros? 

#10# DESAFIO
# Suponha que você planeja uma viagem para o RJ com mais 1 pessoa de 5 diárias nos...
# proximos 30 dias. Você  e seu grupo têm alguns critérios de escolha: 
# - Vocês querem ficar em Ipanema, Copacabana ou Leblon.
# - Vocês preferem que o host esteja no mesmo bairro.  
# - Não desejam pagar um depósito de segurança;
# - Querem um apartamento inteiro só para vocês que seja "instant bookable"
# - A diária já inclua duas pessoas
# Filtre os anúncios que atendem aos critérios acima e crie uma coluna chamada preco_total_viagem,
# com a formula sendo:  taxa de limpeza + preço da diaria x quantidade de diarias.
# Compare os resultados com os do site.
# Dica: Comece com o código abaixo, selecionando as colunas importantes 

