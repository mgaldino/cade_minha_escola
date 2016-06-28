## consulta CNPJ Receita através de API

## exemplo com um caso

url <- "http://receitaws.com.br/v1/cnpj/" # url da api
cnpj <- "63025530001429" 
url_final <- paste(url, cnpj, sep="")
library(jsonlite)
dat <- fromJSON(cnae)$atividade_principal # consulta o cnpj, e retorna em df (resultado da consulta no site é em json)

# para fazer para vários CNPJS, é preciso fazer um loop.
# Aqui, fiz um exemplo com um data.frame com 5 cpnjs. Usei o mesmo cpnj, mas com dados reais o cpnj será diferente

## carrega bibliotecas
library(jsonlite)
library(dplyr)

## cria banco de dados fake, para exemplo
df <- data.frame(id= 1:5, cnpj = rep("63.025.530/0014-29", 5))

## limpa o cnpj, para ficar no formato necessário, sem ponto, barra ou traço
## estou usando o pipe operator (aqui, "%>%"), e a bilioteca dplyr.

df <-  df %>%
  mutate(cnpj = gsub("/", "", cnpj),
         cnpj = gsub("\\.", "", cnpj),
         cnpj = gsub("-", "", cnpj),
         url_cnpj = paste(url, cnpj, sep=""))

url <- "http://receitaws.com.br/v1/cnpj/"

cnae <- readLines(df$url_cnpj[1], warn = "F")
final_df <- fromJSON(cnae)$atividade_principal

for ( i in 2:nrow(df)) {
  cnae <- readLines(df$url_cnpj[i], warn = "F")
  final_df <- rbind_list(final_df, fromJSON(cnae)$atividade_principal)
  Sys.sleep(.1) # pra não afetar o servidor
}
