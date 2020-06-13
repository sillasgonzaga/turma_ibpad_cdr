library(tidyverse)

df <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_series_temporais/master/data/Bike-Sharing-Dataset/day.csv")

glimpse(df)

df_transf <- df %>% 
  # remover colunas irrelevantes
  select(-c(instant, workingday)) %>% 
  # renomear algumas colunas
  rename(
    estacao = season,
    total = cnt,
    year = yr, 
    month = mnth
  ) %>% 
  # mudar weekday, que começa a contar do zero
  mutate(weekday = weekday + 1) %>% 
  # transformar a variavel de feriado para texto
  mutate(holiday = as.character(holiday)) %>% 
  # mudar os valores de algumas variaveis
  mutate(
    # substituir o codigo do ano  pelo ano real
    year = lubridate::year(dteday),
    # adicionar um leading zero no mês
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    # converter weathersit para variavel do tipo factor
    weathersit = factor(weathersit,
                        levels = 1:4,
                        labels = c("muito bom", "bom", "ruim", "muito ruim")),
    # converter dia da semana para variavel do tipo factor
    weekday = factor(weekday, 
                     levels = 1:7,
                     labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")),
    # fazer o mesmo para estacao
    estacao = factor(estacao, 
                     levels = 1:4,
                     labels = c("Inverno", "Primavera", "Verao", "Outono")),
                     #labels = c("Primavera", "Verao", "Outono", "Inverno")),
    # converter colunas numericas para escala normal (não-normalizada)
    temp = temp * 41,
    atemp = atemp * 50,
    hum = hum * 100,
    windspeed = windspeed * 67
  )


glimpse(df_transf)


df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line() +
  # adicionar curva de tendencia
  geom_smooth(se = FALSE) +
  theme_bw() +
  # quebrar eixo x em 1 mes
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%Y",
               minor_breaks = NULL) +
  # inverter eixos
  theme(axis.text.x = element_text(angle = 90))

df_transf %>% 
  select(estacao, year, dteday) %>% 
  group_by(estacao, year) %>% 
  summarise(primeira_data = min(dteday))

?lag

df_mudanca_estacoes <- df_transf %>% 
  select(dteday, estacao) %>% 
  mutate(estacao_dia_anterior = lag(estacao, n = 1),
         estacao_dia_seguinte = lead(estacao, n = 1)) %>% 
  filter(estacao != estacao_dia_anterior)



df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line() +
  # adicionar curva de tendencia
  geom_smooth(se = FALSE) +
  # adicionar retas verticais das estações
  # geom_vline(data = df_mudanca_estacoes,
  #            aes(xintercept = dteday),
  #            linetype = "dashed")
  geom_vline(xintercept = df_mudanca_estacoes$dteday,
             linetype = "dashed") +
  theme_bw() +
  # quebrar eixo x em 1 mes
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%Y",
               minor_breaks = NULL) +
  # inverter eixos
  theme(axis.text.x = element_text(angle = 90))


### colorir linha de acordo com a estacao


df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line(aes(color = estacao, group = NA)) +
  # adicionar curva de tendencia
  geom_smooth(se = FALSE) +
  theme_bw() +
  # quebrar eixo x em 1 mes
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m/%Y",
               minor_breaks = NULL) +
  # inverter eixos
  theme(axis.text.x = element_text(angle = 90))


# - Dia da semana & Feriado
df_transf %>% 
  ggplot(aes(x = weekday, y = total, fill = holiday)) +
  geom_boxplot()

df_transf %>% 
  ggplot(aes(x = weekday, y = casual, fill = holiday)) +
  geom_boxplot()


df_transf %>% 
  ggplot(aes(x = weekday, y = registered, fill = holiday)) +
  geom_boxplot()

summary(df_transf$casual)
summary(df_transf$registered)


df_transf %>% 
  ggplot(aes(x = weathersit, y = total)) +
  geom_boxplot()

df_transf %>% 
  ggplot(aes(x = estacao, y = total, fill = month)) +
  geom_boxplot()

# mes e ano
df_transf %>% 
  ggplot(aes(x = month, y = total, fill = as.character(year))) +
  geom_boxplot()

# estacao e ano
df_transf %>% 
  ggplot(aes(x = estacao, y = total, fill = as.character(year))) +
  geom_boxplot()


#### correlacao

# cor entre par de vetores
cor(df_transf$total, df_transf$temp)
# cor de uma matriz numerica
df_transf %>% 
  select(hum, windspeed, total, temp, atemp) %>% 
  cor()

df_transf %>% 
  ggplot(aes(x = temp, y = atemp)) +
  geom_point()

df_transf <- df_transf %>% 
  select(-atemp)

# grafico de matriz de correlação
library(GGally)
df_transf %>% 
  select_if(is.numeric) %>% 
  select(-c(year, casual, registered)) %>% 
  GGally::ggpairs(progress = FALSE)


df_transf %>% 
  ggplot(aes(x = temp, y = total)) +
  geom_point(aes(color = estacao)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(breaks = scales::breaks_width(1000)) 

# regressao simples
modelo.simples <- lm(total ~ temp, data = df_transf)

modelo.simples

# Y = B0 + B1 * X
# total = 1215 + 162 * temp

1215 + 162 * 30


# regressao multiplta
modelo2 <- lm(total ~ temp + windspeed, data = df_transf) 
modelo2
  
# Y = B0 + B1*X1 + B2*X2
# total = 1991 + 156 * temp - 52 * windspeed
modelo.multiplo <- lm(total ~ . - dteday - casual - registered,
                      data = df_transf)
# desligar notação cientifica
options(scipen = 999)
modelo.multiplo

summary(modelo.multiplo)
modelo.simples
summary(modelo.simples)

head(residuals(modelo.multiplo))

# distruibuicao dos residuos
tibble(residuos = resid(modelo.multiplo)) %>% 
  ggplot(aes(x = residuos)) +
  geom_histogram()


tibble(residuos = resid(modelo.multiplo)) %>% 
  mutate(data = df_transf$dteday) %>% 
  ggplot(aes(x = data, y = residuos)) +
  geom_line()

# investigar onde ocorreu o menor valor de 'total'
df_transf %>% 
  select(dteday, total) %>% 
  arrange(total)


summary(modelo.simples)
summary(modelo.multiplo)

# modificar o dataframe para incluir novas variaveis
df_transf_2 <- df_transf %>% 
    mutate(
      sandy = ifelse(dteday == as.Date("2012-10-29"), "1", "0"),
      total_dia_anterior = lag(total, 1)
    )

modelo.multiplo.2 <- lm(total ~ . - dteday - casual - registered,
                        data = df_transf_2)

summary(modelo.multiplo.2)
summary(modelo.multiplo)

tail(df_transf)
glimpse(df_transf_2)

