#### aula 3 - graficos ----
library(tidyverse)

data(mtcars)
?mtcars

ggplot(data = mtcars)

df_airbnb <- read_csv('listings.csv.gz')
df_airbnb <- df_airbnb %>% 
  mutate(price = parse_number(price))

glimpse(df_airbnb)


ggplot(df_airbnb) +
  geom_point(aes(x = longitude, y = latitude))

# mapear um valor absoluto a um componente estetico
ggplot(df_airbnb) +
  geom_point(aes(x = longitude, y = latitude),
             color = "red")
# mapear a cor dos pontos em função de uma variável
ggplot(df_airbnb) +
  geom_point(aes(x = longitude, y = latitude,
                 color = log(price)))

df_airbnb %>% 
  select(price) %>% 
  mutate(price_log = log(price))

# exemplo de grafico de pontos com gapminder
library(gapminder) # install.packages("gapminder")

gapminder
?gapminder

gapminder %>% 
  filter(year == 2007) %>% 
  ggplot() +
  geom_point(aes(x = log(gdpPercap), y = lifeExp,
                 color = continent,
                 size = pop))

# no grafico de pontos, adicionar uma camada da reta de regressão
gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "lm")


?

df_airbnb %>% 
  count(neighbourhood) %>% 
  top_n(n = 10, wt = n) %>% 
  ggplot(aes(y = neighbourhood, x = n)) +
  geom_col()

# grafico do top 20 países em expec de vida em 2017 por continent
gapminder %>% 
  filter(year == 2007) %>% 
  top_n(20, wt = lifeExp) %>% 
  ggplot(aes(x = lifeExp, y = country, fill = continent)) +
  geom_col()
  

min(gapminder$year)
max(gapminder$year)

gapminder %>% 
  filter(year %in% c(1952, 2007)) %>% 
  group_by(year, continent) %>% 
  summarise(expec_media = mean(lifeExp)) %>% 
  ggplot(aes(x = continent, y = expec_media,
             fill = as.character(year))) +
  geom_col(position = "dodge")




  
  

