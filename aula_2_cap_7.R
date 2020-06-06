library(stringr)


a <- 'texto 1'
b <- 'texto 2'
c <- 'texto 3'

a
b
c

paste0(a, b, c, sep = "_")
paste(a, b, c, sep = "_")

nomes <- c("Rodrigo", "Fulano", "Ciclano")
sobrenomes <- c("Silva", "Almeida", "Teixeira")


paste(nomes, sobrenomes, sep = "@")

cnae.texto <- c('10 Fabricação de produtos alimentícios',
                '11 Fabricação de bebidas',
                '12 Fabricação de produtos do fumo',
                '13 Fabricação de produtos têxteis',
                '14 Confecção de artigos do vestuário e acessórios',
                '15 Preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados',
                '16 Fabricação de produtos de madeira',
                '17 Fabricação de celulose, papel e produtos de papel')

cnae.texto
?str_sub

# usando str_sub
# extrair o codigo
str_sub(cnae.texto, 1, 2)
# extrair o textoe
str_sub(cnae.texto, 4)

vetor_datas <- c("31/01/2020", "01/09/1993", "25/12")

str_sub(vetor_datas, 0, 2)
# mes
str_sub(vetor_datas, 4, 5)
# ano
str_sub(vetor_datas, 7)

vetor_datas[0]



# substituir
telefones <- c('11-9931-9572', '8591-5772', '8562-1923')
?str_replace

str_replace(telefones, "-", '')
str_replace_all(telefones, "-", '@')


meu_nome <- "Fulano"

x <- c("Prazer, sou o {meu_nome}")

str_glue(x)

nomes
sobrenomes

str_glue("Oi, meu nome é {nomes} de sobrenome {sobrenomes}")


# buscar corresponderncias
telefones
?str_count
str_count(telefones, "-")
str_count(telefones, "5")

nomes
str_count(nomes, "a")


str_detect(nomes, "a")


str_starts(nomes, 'R')


cpfs <- c(1234, 01833827570, 45614814570, 4, 4000001111)
cpfs

# padding
str_length(cpfs)
str_pad(cpfs, width = 11, side = 'left', pad = "0")


x <- c("      inicio",
       "final      ",
       "      ambos      ",
       "    no       meio        ",
       "não remover")

tibble(var = c("A", 'B', ' A')) %>% 
  mutate(var_limpo = str_squish(var)) %>% 
  filter(var_limpo == 'A')


str_squish(x)



textos <- c("Fulano", "fulano", "abcdeF", "01584", 
            "abc456", "123def", "OI", "meuemail@gmail.com",
            "www.google.com", "Meu nome é Fulano")

textos

str_detect(textos, "F")

str_subset(textos, regex("^F", ignore_case = TRUE))

str_subset(textos, "\\.")

str_subset(textos, "\\.")


remotes::install_github("sillasgonzaga/literaturaBR")
# é equivalente a 
library(remotes)
install_github("sillasgonzaga/literaturaBR")
