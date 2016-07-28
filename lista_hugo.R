## Gerando base ciadades para pedido LAI
## info sobre fiscal da obra, cronograma financeiro e de execução
## e relatório do fiscal.
#' function to convert character vectors to UTF-8 encoding
#'
#' @param x the vector to be converted
#' @export 
x <- c("foo", "bar")
iconv(x,"UTF-8","UTF-16LE")


toUTF8 <- function(x){
    worker <- function(x){
      iconv(x, from = Encoding(x), to = "UTF-8")
    }
    unlist(lapply(x, worker))
  }



#' function to write csv files with UTF-8 characters (even under Windwos)
#' @param df data frame to be written to file
#' @param file file name / path where to put the data
#' @export 

write_utf8_csv <- function(df, file){
    firstline <- paste(  '"', names(df), '"', sep = "", collapse = " , ")
    char_columns <- seq_along(df[1,])[sapply(df, class)=="character"]
    for( i in  char_columns){
      df[,i] <- toUTF8(df[,i])
    }
    data <- apply(df, 1, function(x){paste('"', x,'"', sep = "",collapse = " , ")})
    writeLines( c(firstline, data), file , useBytes = T)
}

setwd("/Users/natalia/Documents/Manoel")
load(file="base_obras.RData")

library(stringr)
library(dplyr)

## salvando arquivo para o hugo
base_obras_hugo <- base_obras %>%
  filter(!(Situação %in% c("Planejamento pelo proponente", "Obra Cancelada", "Em Reformulação"))) %>%
  filter(!is.na(termo_convenio)) %>%
  filter(!is.na(situacao_termo_convenio)) %>%
  rename(cidade_uf = `Município - UF`) %>%
  mutate(uf = gsub("- ", "", str_extract(cidade_uf, "-\\ [A-Z]+")))


aux <- base_obras_hugo %>%
  group_by(Situação, uf) %>%
  do(sample_n(.,2, replace=T)) %>%
  ungroup() %>%
  distinct(id)

setwd("/Users/natalia/Documents/Manoel/Escolas/R")
write.table(aux, file="amostra_pedido_lai_v2.csv", row.names=F, sep=";",
            fileEncoding="UTF-8-MAC")

write_utf8_csv(aux, file="amostra_pedido_lai_v3.csv")
