library(tidyverse)

table1
table2
table3
table4a
table4b
table5

data("USArrests")
USArrests
glimpse(USArrests)

# criar uma coluna a partir dos row.names
USArrests$state <- row.names(USArrests)
# EXCLIR OS ROW.NAMES
row.names(USArrests) <- NULL
USArrests

USArrests %>% gather(key = "indicador_crime",
                     value = 'valor',
                     c(Murder, Assault, UrbanPop, Rape)) %>% 
  arrange(state)

table1 %>% 
  gather(key = "tipo",
         value = "quantidade",
         cases, population) %>% 
  arrange(country, year)

# pivot_longer()
USArrests %>% 
  pivot_longer(cols = c(Murder, Assault, UrbanPop, Rape),
               names_to = "indicador_crime",
               values_to = 'valor')

# outras maneiras de definir o argumento cols()
USArrests %>% 
  # sintaxe: nome da primeira coluna : nome da ultima da coluna da sequencia
  pivot_longer(cols = Murder:Rape)


USArrests %>% 
  # mudar a ordem das colunas
  select(Murder, Assault, state, UrbanPop, Rape) %>% 
  # sintaxe cols: definir que coluna vc NÃO QUER transpor
  # cols = -nome_coluna
  pivot_longer(cols = -state)

#### exemplo do spread
?spread
table2
table1

table2 %>% spread(key = type,
                  value = count)

?separate
table3 %>% separate(col = rate,
                    into = c("casos", "populacao"),
                    sep = "/",
                    convert = TRUE)

table5
?unite

table5 %>% unite(col = ano_com_seculo,
                 century, year, rate,
                 sep = "@",
                 remove = FALSE)

### exemplo de separate com dados sujos
table3

tibble(coluna = c("a/b/c", "a/b/c/d", 'a/b', "a/b/c", 'a/b')) %>% 
  separate(col = coluna,
           into = c("c1", "c2", "c3"),
           sep = "/",
           extra = "merge",
           fill = "left")


tibble(coluna = c("a/b/c", "a/b/c/d", 'a/b', "a/b/c", 'a/b')) %>% 
  mutate(qtd_separadores = str_count(coluna, "/")) %>% 
  filter(qtd_separadores == 2)


?separate

exemplo <- tibble(grupo = c("a", "a", "b","b"),
                  y = c("1, 2", "3;4", "1,2,3", "4"))

exemplo
?separate_rows

exemplo %>% separate_rows(y,
                          sep = ",|;",
                          convert = TRUE)


dados2016 <- tibble(ano = c(2016, 2016, 2016), 
                        valor = c(938, 113, 1748), 
                        produto = c('A', 'B', 'C'))

dados2017 <- tibble(valor = c(8400, 837, 10983), 
                        produto = c('H', 'Z', 'X'),
                        ano = c(2017, 2017, 2017))

dados2016
dados2017

bind_rows(dados2016, dados2017)

# exemplo do capitulo anterior
table4a
table4b
# concatenar verticalmente
bind_rows(table4a, table4b)
# concatenar horizontalmente
bind_cols(table4a, table4b)


# funcoes join
band_members
band_instruments
band_instruments2

# juntar os dataframes de membros e instrumentos
# sem especificar o by
inner_join(band_members, band_instruments, by = "name")
inner_join(band_members, band_instruments2, by = c("name" = "artist"))


band_members$sobrenome <- c("Jagger", "A", "B")
band_members
band_instruments$sobrenome <- c("A", "B", "C")
band_instruments


inner_join(band_members, band_instruments, by  = c("name"))

# left join
band_members
left_join(band_members, band_instruments)

# exercicio left join vs inner join
vendedores <- tibble(codigo = c(1, 2, 3),
                     nome = c("A", "B", 'C'))

vendas_2020 <- tibble(codigo = c(1, 3),
                      total = c(1000, 2000))


inner_join(vendedores, vendas_2020)

left_join(vendedores, vendas_2020) %>% 
  mutate(total = replace_na(total, 0))


# a ordem importa no left_join
left_join(band_members, band_instruments)


left_join(band_instruments, band_members)


band_members
band_instruments

full_join(band_members, band_instruments)


semi_join(band_members, band_instruments)
# é equivalente a um filter
band_members %>% filter(name %in% band_instruments$name)


anti_join(band_members, band_instruments)






