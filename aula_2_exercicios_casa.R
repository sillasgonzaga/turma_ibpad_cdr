library(tidyverse)
library(nycflights13)
#### exercicio 6.3 ----

# questao 1
glimpse(flights)
glimpse(airports)

airports %>% filter(faa == "EWR")


df_nome_aeroportos <- airports %>% select(faa, name)
df_nome_aeroportos

# group_by() e summarise()
flights %>% 
  group_by(origin, dest) %>% 
  summarise(qtd_voos = n()) %>% 
  arrange(desc(qtd_voos)) %>% 
  left_join(df_nome_aeroportos, by = c("origin" = "faa")) %>% 
  rename(nome_aeroporto_origem = name) %>% 
  left_join(df_nome_aeroportos, by = c("dest" = "faa")) %>% 
  rename(nome_aeroporto_destino = name) %>% 
  select(nome_aeroporto_origem, nome_aeroporto_destino, qtd_voos)

# questao 2
participantes <- tibble(
  Nome = c('Carlos', 'Maurício', 'Ana Maria', 'Rebeca', 'Patrícia'),
  Estado = c('Brasília', 'Minas Gerais', 'Goiás', 'São Paulo', 'Ceará'),
  Idade = c(23, 24, 22, 29, 28)
)

aprovados <- tibble(
  Nome = c('Carlos', 'Patrícia'),
  Pontuacao = c(61, 62)
)

eliminados <- tibble(
  Nome = c('Maurício', 'Ana Maria', 'Rebeca'),
  Pontuacao = c(49, 48, 48)
)

glimpse(participantes)
participantes
aprovados
eliminados

left_join(participantes, aprovados, by = "Nome") %>% 
  left_join(eliminados, by = "Nome") %>% 
  mutate(Pontuacao = ifelse(is.na(Pontuacao.x), 
                            Pontuacao.y,
                            Pontuacao.x))


part_aprovados <- inner_join(participantes, aprovados, by = "Nome")

part_aprovados <- part_aprovados %>% 
  mutate(Resultado = "Aprovado")

part_reprovados <- inner_join(participantes, 
                              eliminados,
                              by = "Nome") %>% 
  mutate(Resultado = "Eliminado")


part_aprovados
part_reprovados

bind_rows(part_aprovados, part_reprovados)


#### exercicio 6.4 ----

# Carregue os pacotes tidyverse e janitor.
library(tidyverse)
library(janitor) # install.packages("janitor")

# questao 1
hero_powers <- read_csv(
  "26532_33799_bundle_archive/super_hero_powers.csv",
  na = c("", "NA", "-")
  )
hero_info <- read_csv(
  "26532_33799_bundle_archive/heroes_information.csv",
  na = c("", "NA", "-")
  )

glimpse(hero_info)
glimpse(hero_powers)

# questao 2
hero_info <- clean_names(hero_info)
hero_powers <- hero_powers %>% clean_names()

glimpse(hero_info)

# questao 3
hero_info <- hero_info %>% select(-x1)

# questao 4 
# pulei

# questao 5

# sintaxe do r base
hero_info$publisher %>% unique()
# sintaxe do tidyverse
hero_info %>% distinct(publisher) %>% print(n = 50)

hero_info %>% 
  mutate(publisher = ifelse(publisher == "Marvel Comics",
                            "Marvel",
                            ifelse(publisher == "DC Comics",
                                   "DC",
                                   "Outros")
                            ))

# case_when

# sintaxe da funcão:
# case_when(teste logico 1 ~ resposta 1, 
#           teste logico 2 ~ resposta 2,
#           ...,
#           TRUE ~ resposta quando nos testes acima deu TRUE)

hero_info <- hero_info %>% 
  mutate(publisher = case_when(
    publisher == "Marvel Comics" ~ "Marvel",
    publisher == "DC Comics" ~ "DC",
    is.na(publisher) ~ "Outros",
    TRUE ~ "Outros"
  ))

# questao 6
hero_info %>% 
  select(publisher, race) %>% 
  filter(!is.na(race)) %>% 
  count(publisher, race) %>% 
  spread(publisher, n, fill = 0) %>% 
  # filtrando personagens exclusivos da Marvel
  filter(Marvel >= 1, DC == 0, Outros == 0)


racas_por_editora <- hero_info %>% 
  select(publisher, race) %>% 
  filter(!is.na(race)) %>% 
  group_by(publisher, race) %>% 
  summarise(qtd_personagens_editora = n())

racas_por_editora

racas_geral <- hero_info %>% 
  group_by(race) %>% 
  summarise(qtd_personagens = n())


racas_por_editora
racas_geral

left_join(racas_por_editora, racas_geral) %>% 
  filter(qtd_personagens == qtd_personagens_editora)

# questao 7
?top_n

hero_info %>% 
  filter(!is.na(gender), !is.na(eye_color)) %>% 
  count(gender, eye_color) %>% 
  #arrange(gender, desc(n)) %>% 
  group_by(gender) %>% 
  top_n(3, wt = n)
  
# questao 8  
hero_powers %>% 
  summarise_if(is.logical, mean)

# questao 9

# sintaxe gather()
hero_powers %>% 
  gather(nome_poder, possui_poder, -hero_names)

# sintaxe pivot_longer()

hero_powers_long <- hero_powers %>% 
  pivot_longer(cols = -hero_names,
               names_to = "nome_poder",
               values_to = "possui_poder")

hero_powers_long %>% 
  group_by(nome_poder) %>% 
  summarise(pct_personagens = mean(possui_poder)) %>% 
  arrange(desc(pct_personagens))

# questao 10
hero <- inner_join(hero_info, hero_powers,
                   by = c("name" = "hero_names"))

# questao 11
hero %>% 
  group_by(publisher) %>% 
  summarise(pct_telepata = mean(telepathy))

# questao 12

# fazendo com top_n()
hero %>% 
  select(name, publisher, flight, weight) %>% 
  filter(flight) %>% 
  top_n(10, wt = weight)

# fazendo com head()
hero %>% 
  select(name, publisher, flight, weight) %>% 
  filter(flight) %>% 
  arrange(desc(weight)) %>% 
  head(10)

# questao 13
write_csv(hero, "herois_completo.csv")





