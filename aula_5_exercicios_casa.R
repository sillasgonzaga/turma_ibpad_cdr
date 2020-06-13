# exercicios aula 5
library(tidyverse)

herois <- read_csv("herois_completo.csv")
herois

#ggplot(herois, ...)

herois %>% 
  ggplot(aes(x = height, y = weight)) +
  geom_point()

# converter as unidades de medida para SI
herois <- herois %>% 
  mutate(height = height/100,
         weight = weight * 0.453592)

herois %>% 
  ggplot(aes(x = height, y = weight)) +
  geom_point()

herois <- herois %>% 
  filter(weight > 0, height > 0)

## q2
library(scales)
ggplot(herois, aes(x = height)) +
  geom_histogram(binwidth = .1) +
  scale_x_continuous(breaks = breaks_width(0.5),
                     minor_breaks = NULL)

# analisar essa concentração de personagens entre 1,5 e 2m
herois %>% 
  filter(height >= 1.5, height <= 2.5) %>% 
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(breaks = breaks_width(0.10))



### q3

# histograma
herois %>% 
  ggplot(aes(x = weight, fill = publisher)) +
  geom_histogram(binwidth = 20) +
  facet_wrap(vars(publisher), scales = "free_y", nrow = 3)

### q4
herois %>% 
  count(publisher) %>% 
  ggplot(aes(x = reorder(publisher, -n),
             y = n)) +
  geom_col() +
  geom_text(aes(label = n),
            vjust = 0)

### q5
herois %>% 
  count(publisher, alignment) %>% 
  filter(!is.na(alignment)) %>% 
  ggplot(aes(x = publisher, y = n, fill = alignment)) +
  geom_col(position = position_dodge())


### q6
herois %>% 
  count(publisher, alignment) %>% 
  filter(!is.na(alignment)) %>% 
  ggplot(aes(x = publisher, y = n, fill = alignment)) +
  geom_col(position = position_fill()) +
  scale_y_continuous(breaks = breaks_width(0.10))

# reproduzir o calculo do position_fill() na mão
herois %>% 
  count(publisher, alignment, name = "qtd_personagens") %>% 
  filter(!is.na(alignment)) %>% 
  group_by(publisher) %>% 
  mutate(n_editora = sum(qtd_personagens),
         proporcao = qtd_personagens/n_editora) %>% 
  ggplot(aes(x = publisher, y = proporcao, fill = alignment)) +
  geom_col()


### q7
glimpse(herois)

hero_agg <- herois %>% 
  pivot_longer(cols = agility:omniscient,
               names_to = "nome_do_poder",
               values_to = "tem_poder") %>% 
  group_by(publisher, name) %>% 
  summarise(qtd_poderes = sum(tem_poder))

hero_agg

### q8

# top 10 no geral
hero_agg %>% 
  ungroup() %>%
  top_n(n = 10, wt = qtd_poderes)

# top 10 por editora
hero_agg %>% 
  #ungroup() %>%
  group_by(publisher) %>% 
  top_n(n = 10, wt = qtd_poderes) %>% 
  arrange(publisher, desc(qtd_poderes)) %>% 
  ggplot(aes(y = name, x = qtd_poderes)) +
  geom_col() +
  facet_wrap(vars(publisher), scales = "free_y", 
             nrow = 3)

# como excluir empates


### q9


### q10

















