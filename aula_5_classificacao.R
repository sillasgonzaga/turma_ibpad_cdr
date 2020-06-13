library(tidyverse)

df_funcionario <- read_csv("rh/employee_survey_data.csv")
df_geral <- read_csv("rh/general_data.csv")
df_in <- read_csv("rh/in_time.csv")
df_gestor <- read_csv("rh/manager_survey_data.csv")
df_out <- read_csv("rh/out_time.csv")

glimpse(df_funcionario)
glimpse(df_geral)
glimpse(df_gestor)

df_in
df_out

df_in_long <- df_in %>% 
  rename(id = X1) %>% 
  pivot_longer(cols = -id,
               names_to = "dia",
               values_to = 'horario_entrada')

df_out_long <- df_out %>% 
  rename(id = X1) %>% 
  pivot_longer(cols = -id,
               names_to = "dia",
               values_to = 'horario_saida')

df_in_long
df_out_long

df_folha_ponto <- left_join(df_in_long, df_out_long)

df_folha_ponto_medio <- df_folha_ponto %>% 
  mutate(tempo_trabalho = as.numeric(horario_saida - horario_entrada)) %>% 
  group_by(id) %>% 
  summarise(tempo_medio_trabalho = mean(tempo_trabalho, na.rm = TRUE))

rh <- df_geral %>% 
  inner_join(df_folha_ponto_medio, by = c("EmployeeID" = "id")) %>% 
  inner_join(df_funcionario, by = "EmployeeID") %>% 
  inner_join(df_gestor, by = "EmployeeID")

glimpse(rh)

#### EDA

# renda mensal
rh %>% 
  ggplot(aes(x = MonthlyIncome, y = Attrition)) +
  geom_boxplot()

# tempo medio no trabalho
rh %>% 
  ggplot(aes(x = tempo_medio_trabalho, y = Attrition)) +
  geom_boxplot()

# estado civil
rh %>% 
  count(MaritalStatus, Attrition) %>%
  group_by(MaritalStatus) %>% 
  mutate(proporcao = n/sum(n))


rh %>% 
  count(BusinessTravel, Attrition) %>%
  group_by(BusinessTravel) %>% 
  mutate(proporcao = n/sum(n))

rh %>% 
  count(Attrition)

mean(rh$Attrition == "Yes")


n
# salvar em um objeto o tamanho vertical do dataframe
n = nrow(rh)
# sortear aleatoriamente 70% das linhas do dataframe para compor
# o conjunto de treino
set.seed(123)
ind_treino <- sample(1:n, size = n * 0.7)
ind_treino
length(ind_treino)
length(1:n)


# criar conjunto de treino
rh_treino <- rh[ind_treino, ]
# criar conjunto de teste a partir da exclusao das linhas do conjunto de treino
rh_teste <- rh[-ind_treino, ]

dim(rh_treino)
dim(rh_teste)

# construir modelo de arvore
library(rpart)
library(rpart.plot)

mod_arvore_simples <- rpart(Attrition ~ . - EmployeeID,
                            data = rh_treino)

mod_arvore_simples

prp(mod_arvore_simples)

rpart.plot::prp(mod_arvore_simples, type = 4,
                extra = 6,
                fallen.leaves = FALSE, varlen = 0,
                faclen = 0, box.palette = "auto")


# inicializar pdf em branco
pdf("minha_primeira_arvore.pdf")
# preencher pdf com o grafico da arvore
rpart.plot::prp(mod_arvore_simples, type = 4,
                extra = 6,
                fallen.leaves = FALSE, varlen = 0,
                faclen = 0, box.palette = "auto")
# destravar a sessao para o pdf ser salvo
dev.off()

rh %>% 
  na.omit() %>% 
  filter(tempo_medio_trabalho >= 8.2,
         TotalWorkingYears < 9,
         MaritalStatus == "Single") %>% 
  summarise(mean(Attrition == "Yes"))


rh %>% 
  ggplot(aes(x = tempo_medio_trabalho, y = TotalWorkingYears)) +
  geom_point(aes(color = Attrition)) +
  geom_vline(xintercept = 8.2, linetype = "dashed") +
  geom_hline(yintercept = 9, linetype = 'dashed')


### previsoes

# predict(objeto do modelo, newdata = ...)
previsao <- predict(mod_arvore_simples, rh_teste, type = 'class')

head(previsao)

tibble(valor_real = rh_teste$Attrition,
       valor_previsto = previsao) %>% 
  count(valor_real, valor_previsto) %>% 
  pivot_wider(id_cols = valor_previsto, 
              names_from = valor_real,
              values_from = n)

# acuracia geral
(1102 + 58)/nrow(rh_teste)
# acuracia nos funcionarios que saíram
58/(58 + 157)
# acuracia nos funcionarios que nao sairam
1102/(1102 + 6)

library(caret)

confusionMatrix(data = previsao,
                reference = as.factor(rh_teste$Attrition),
                positive = "Yes")

### modelo randomForest
library(randomForest)

rh_treino <- na.omit(rh_treino)

rh_treino <- rh_treino %>% 
  mutate_if(is.character, as.factor)

mod_rf <- randomForest(Attrition ~ . - EmployeeID,
                       data = rh_treino,
                       importance = TRUE)

rh_teste <- rh_teste %>% 
  mutate_if(is.character, as.factor)


previsao_rf <- predict(mod_rf, rh_teste)




confusionMatrix(data = previsao_rf,
                reference = as.factor(rh_teste$Attrition),
                positive = "Yes")

181/(181 + 29)

varImpPlot(mod_rf,
           # especificar que a medida de importancia é a perda media de acuracia
           type = 1,
           main = "Importância de cada variável explan. no modelo de RF")


### exercicios
library(ISLR)
data(Wage)
glimpse(Wage)

Wage %>% 
  ggplot(aes(x = logwage)) +
  geom_histogram()

ind_treino_wage <- sample(1:nrow(Wage), 
                          size = .70 * nrow(Wage),
                          replace = FALSE)

wage_treino <- Wage[ind_treino, ]
wage_test <- Wage[-ind_treino, ]

mod_regressao <- lm(logwage ~ health + education + age, 
                    data = wage_treino)

summary(mod_regressao)

# previsao pro set de teste
previsao_salario <- predict(mod_regressao, wage_test)

y_real = wage_test$logwage

mean(abs((previsao_salario - y_real)/y_real))






