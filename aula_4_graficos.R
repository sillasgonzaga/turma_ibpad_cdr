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

dados_2017 %>% 
  ggplot(aes(y = lifeExp)) +
  geom_boxplot()

quantile(dados_2017$lifeExp)

# analisar a distribuição de uma var numerica em função de
# uma var categorica
dados_2017 %>% 
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_histogram(binwidth = 5, boundary = 0)


dados_2017 %>% 
  ggplot(aes(y = lifeExp, x = continent)) +
  geom_boxplot()

dados_2017 %>% 
  filter(continent == "Americas") %>% 
  arrange(lifeExp)


gapminder %>% 
  filter(year == min(year) | year == max(year)) %>% 
  ggplot(aes(x = continent,
             y = lifeExp,
             fill = as.character(year))) +
  geom_boxplot()



gapminder %>% 
  filter(year == min(year) | year == max(year)) %>% 
  ggplot(aes(x = continent,
             y = pop,
             fill = as.character(year))) +
  geom_boxplot() +
  scale_y_log10()


dados_2017 %>%
  filter(continent != "Oceania") %>% 
  ggplot(aes(y = lifeExp, x = continent)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = 0.07)

# dot plot

dados_2017 %>% 
  filter(continent == "Americas") %>% 
  ggplot(aes(y = fct_rev(country), x = lifeExp)) +
  geom_point()


gapminder %>% 
  filter((year == min(year) | year == max(year)) & 
        continent == "Americas") %>% 
  ggplot(aes(x = lifeExp, y = fct_rev(country))) +
  geom_line() +
  geom_point(aes(color = as.character(year)))
  

gapminder %>% 
  filter((year == min(year) | year == max(year)) & 
           continent == "Americas") %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line()

ggsave("meu_grafico.png")

#### texto
?geom_text()


dados_2017 %>% 
  ggplot(aes(x = log(gdpPercap), 
             y = log(pop),
             label = country)) +
  geom_point() +
  geom_text(data = dados_2017 %>% filter(continent == "Africa",
                                         lifeExp <= 50))

gapminder %>% 
  group_by(year) %>% 
  summarise(expec_media = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = expec_media)) +
  geom_col() +
  geom_text(aes(label = round(expec_media, 1)),
            vjust = 1)


gapminder %>% 
  group_by(year) %>% 
  summarise(expec_media = mean(lifeExp)) %>% 
  ggplot(aes(x = year, y = expec_media)) +
  geom_col() +
  geom_label(aes(label = round(expec_media, 1)),
            vjust = 1)


#### geom_segment() ----

dados_2017 %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point()  +
  # destacar o brasil com uma reta
  geom_segment(x = 9000, xend = 9000,
               y = 60, yend = 72.3,
               color = 'red',
               arrow = arrow(length = unit(0.03, "npc"))) +
  # acrescentar camada de texto
  geom_text(x = 9000, y = 58, label = "Brasil",
            color = "red")


dados_2017 %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point() +
  # reta vertical
  geom_vline(xintercept = mean(dados_2017$gdpPercap),
             linetype = "dashed") +
  # reta horizontal
  geom_hline(yintercept = quantile(dados_2017$lifeExp),
             linetype = "dashed")


mean(dados_2017$lifeExp)
mean(dados_2017$gdpPercap)


dados_2017 %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point() +
  geom_rect(xmin = 20000, xmax = 30000,
            ymin = 70, ymax = 80,
            alpha = .2,
            inherit.aes = FALSE)


dados_2017 %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point() +
  annotate(geom = 'rect',
           xmin = 20000, xmax = 30000,
           ymin = 70, ymax = 80,
           alpha = .4) +
  annotate(geom = 'rect',
           xmin = 0, xmax = 5000,
           ymin = -Inf, ymax = Inf,
           alpha = .4,
           fill = "purple")

cores_continentes <- c("Americas" ="purple",
                       "Asia" = "red",
                      "Europe" ="black",
                      'Africa' = "green",
                      "Oceania" = "yellow")
cores_continentes

#### scale_color_manual ()
dados_2017 %>% 
  ggplot(aes(x = log(gdpPercap),
             y = lifeExp,
             color = continent)) +
  geom_point() +
  scale_color_manual(
    values = cores_continentes
    )

# modificando os eixos
library(scales)
dados_2017 %>% 
  ggplot(aes(x = log(gdpPercap),
             y = lifeExp,
             color = continent)) +
  geom_point() +
  scale_y_continuous(breaks = breaks_width(5),
                     minor_breaks = NULL) +
  scale_x_continuous(minor_breaks = NULL,
                     name = "PIB Per capita")

# modificar legendas no grafico
library(scales)
dados_2017 %>% 
  ggplot(aes(x = log(gdpPercap),
             y = lifeExp,
             color = continent)) +
  geom_point() +
  labs(
    x = "USD por ano",
    y = "Anos",
    color = NULL,
    title  = "Expectativa de vida em função do PIB per capita",
    subtitle = "Existe uma relação entre as variáveis",
    caption = "Curso IBPAD")





?labs


gapminder %>% 
  group_by(continent) %>% 
  summarise(expec_media = mean(lifeExp)) %>% 
  ggplot(aes(x = continent, y = expec_media)) +
  geom_col() +
  geom_text(aes(label = round(expec_media, 1)),
            vjust = 1) +
  scale_x_discrete(labels = c("África", 'Américas', "Ásia",
                              "Europa", "Oceania"))

library(ISLR)

ggplot(Default, aes(x = default, y = balance)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Calote",
                   limits = c("Yes", "No"),
                   labels = c("Sim", "Não")) +
  labs(y = "Valor devido médio após o pagamento mensal")


data("economics")
?economics
head(economics)

library(lubridate)

economics %>% 
  #filter(year(date) == 1968) %>% 
  ggplot(aes(x = date, y = uempmed)) +
  geom_line() +
  scale_x_date(date_breaks = "4 years",
               date_labels = '%Y',
               minor_breaks = NULL)


# quebras de 1 mês
economics %>% 
  filter(year(date) == 1968) %>% 
  ggplot(aes(x = date, y = uempmed)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month",
               date_labels = '%d-%b/%y',
               minor_breaks = NULL)


e




dados_2017 %>%
  mutate(cor_dos_pontos = ifelse(lifeExp < 70,
                                 'purple',
                                 'green')) %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = cor_dos_pontos)) +
  geom_point() +
  scale_color_identity()

# colorir pontos de acordo com variavel numerica
?scale_color_brewer
# variaveis continuas: distiller
# variaveis categorias: brewer

dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = lifeExp)) +
  geom_point() +
  scale_color_distiller(palette = "RdYlGn",
                        direction = 1)

dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = lifeExp)) +
  geom_point() +
  scale_color_viridis_c()


dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point(aes(color = continent)) +
  scale_color_brewer(palette = 'Set1')

#### facets ----

dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point(aes(color = continent)) +
  facet_wrap(vars(continent),
             scales = "free",
             nrow = 1)

dados_2017 %>% 
  group_by(continent) %>% 
  summarise(m = mean(lifeExp)) %>% 
  ggplot(aes(x =  continent, y = m)) +
  geom_col(fill = "navyblue")



dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point() +
  theme_bw()

dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point() +
  theme_minimal()
  
library(ggthemes)


dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point() +
  theme_economist()

?theme()

dados_2017 %>% 
  group_by(continent) %>% 
  summarise(m = mean(lifeExp)) %>% 
  ggplot(aes(x =  continent, y = m)) +
  geom_col(fill = "navyblue") +
  theme(axis.text.x = element_text(color = 'red',
                                   angle = 90))

# install.packages('ggThemeAssist')



dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point() +
  theme(panel.grid.major = element_line(colour = "gray29", 
    linetype = "dashed"), panel.background = element_rect(fill = "gold1", 
    colour = "antiquewhite1"), plot.background = element_rect(fill = "antiquewhite2", 
    colour = "aquamarine", size = 1.2, linetype = "solid"))


#### capitulo 13 -----
grafico <- dados_2017 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(nome_pais = country))

library(plotly)

grafico
ggplotly(grafico, tooltip = "nome_pais")


dados_2017 %>% 
  group_by(continent) %>% 
  summarise(m = mean(lifeExp)) %>% 
  ggplot(aes(x =  continent, y = m)) +
  geom_col(fill = "navyblue") +
  theme(axis.text.x = element_text(color = 'red',
                                   angle = 90))

ggplotly()



