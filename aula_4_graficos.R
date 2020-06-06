library(tidyverse)
library(gapminder)

head(gapminder)

gapminder

# qual o ano mais recente nos dados
max(gapminder$year)

gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point()

# o codigo acima é equivalente a
gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "blue") +
  scale_x_log10()

gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  scale_x_log10()

# destacar brasil no grafico
dados_2017 <- gapminder %>% 
  filter(year == max(year)) 

brasil_2017 <- dados_2017 %>% filter(country == "Brazil")
brasil_2017

ggplot(dados_2017, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "gray", size = 2) +
  # acrescentar uma camada apenas com o df do brasil
  geom_point(data = brasil_2017, color = "red")

# colorir pontos de acordo com continente
gapminder %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()

# resumo de grafico de barras
dados_2017 %>% 
  group_by(continent) %>% 
  summarise(qtd_paises = n()) %>% 
  ggplot(aes(x = continent, y = qtd_paises)) +
  geom_col(color = 'red', fill = 'blue')

# reordenar as barras de acordo com outra variavel
dados_2017 %>% 
  group_by(continent) %>% 
  summarise(qtd_paises = n(),
            expec_vida_media = mean(lifeExp)) %>% 
  arrange(desc(expec_vida_media)) %>% 
  ggplot(aes(x = continent, y = expec_vida_media)) +
  geom_col()

# entendendo factors
vetor_factors <- factor(c("Fulano", "Ciclano", "Fulano"))
vetor_characters <- c("Fulano", "Ciclano", "Fulano")

vetor_factors
vetor_characters

levels(vetor_factors)
levels(dados_2017$continent)

vetor_factors
as.integer(vetor_factors)

# modificar a coluna continent (factor)
# de forma que a ordem dos levels seja referente a expec
# de vida
dados_por_continente <- dados_2017 %>% 
  group_by(continent) %>% 
  summarise(qtd_paises = n(),
            expec_vida_media = mean(lifeExp))

dados_por_continente
levels(dados_por_continente$continent)

dados_por_continente <- dados_por_continente %>% 
  # modificar os levels da coluna continent
  mutate(continent = reorder(continent, -expec_vida_media))

levels(dados_por_continente$continent)

dados_por_continente %>% 
  ggplot(aes(x = continent, y = expec_vida_media)) +
  geom_col()


# grafico de linhas
gapminder %>% 
  filter(country == "Brazil") %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line() +
  geom_point()

# modificando atributos esteticos do geom_line
gapminder %>% 
  filter(country == "Brazil") %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_line(
    alpha = 0.3,
    #color = "green"
    #group = ,
    #linetype = "dotted",
    size = 5
  )
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, group = country)) +
  geom_line()

# calcular a expec media de vida por continente por ano
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(expec_media = mean(lifeExp)) %>% 
  ggplot(aes(x = year, 
             y = expec_media,
             color = continent)) +
  geom_line() +
  geom_point()

#### histograma
dados_2017

dados_2017 %>% 
  ggplot(aes(x = lifeExp)) +
  geom_histogram(color = 'red')

# modificando parametros do histograma

dados_2017 %>% 
  ggplot(aes(x = lifeExp)) +
  geom_histogram(color = 'red', binwidth = 5, boundary = 0)

# desligar a notação cientifica:
options(scipen = 999)

dados_2017 %>% 
  ggplot(aes(x = log(pop))) +
  geom_histogram(color = 'red')

# fazer histograma na mão com geom_col() e cut()

summary(round(dados_2017$lifeExp))

seq(from = 40, to = 85, by = 5)

cut(round(dados_2017$lifeExp),
    breaks = seq(from = 40, to = 85, by = 5),
    include.lowest = TRUE)

dados_2017 %>% 
  mutate(intervalo = cut(
    round(lifeExp),
    breaks = seq(from = 40, to = 85, by = 5),
    include.lowest = TRUE)
    ) %>% 
  group_by(intervalo) %>% 
  summarise(qtd = n()) %>% 
  ggplot(aes(x = intervalo, y = qtd)) +
  geom_col()


























