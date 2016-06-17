## webscrapping

## primeiro, pegando lista de obras

setwd("/Users/natalia/Documents/Manoel")

# arquivo com lista de obras
escolas <- read.table("Escolas_Brasil_v3.csv", header=T, as.is=T, sep = ",",  quote="\"", na.strings = "NA",
                      fileEncoding = "UTF-8") 
# coletando id das obras
library(dplyr)
library(tidyr)
library(stringr)
library(XML)
library(RCurl)

escolas <- escolas %>%
  mutate(id = str_extract(Nome_Obra, "\\([0-9]+\\)"),
         id = gsub("\\(", "", id),
         id = gsub("\\)", "", id))
  
head(escolas)

library(RCurl)
library(XML)
setInternet2(TRUE)
options(timeout=500)

## função que pega dados das contratações
getConstructionData <- function(url) {
  html <- getURL(url)
  doc <- htmlParse(html)   # parseia url
  linksAux <- xpathSApply(doc, "//dt", xmlValue)   # coleta os links
  linksAux1 <- xpathSApply(doc, "//dd", xmlValue) 
  df <- data.frame(linksAux, linksAux1)
}

## cria as urls para passar no loop
url <- "http://simec.mec.gov.br/painelObras/contratacao.php?obra="
url_final <- paste(url, escolas$id, sep="")

## loop para pegar todas as obras
lista_df <- list()
n <- length(url_final)
for ( i in 1:n) {
  lista_df[[i]] <- getConstructionData(url_final[i])
  Sys.sleep(.5) # para não derrubar o site
  if(i %% 1000 == 0) print(i)
}

## adiciona id, e corrige umas infos
length(lista_df)
i <- 1
while ( i <= n) {
  if(i %in% c(6161, 9178, 11296)) { # checa se são ids que a busca não retornou escolas
    i <- i+1
  }
  lista_df[[i]]$id <- escolas$id[i] # adiciona id das escolas
  aux <- which(duplicated(lista_df[[i]]$linksAux)) # as vezes tem meúltiplas linhas iguais, torna elas diferentes
  lista_df[[i]]$linksAux <- as.character( lista_df[[i]]$linksAux)
  lista_df[[i]]$linksAux[aux] <- paste(lista_df[[i]]$linksAux[aux], 1:length(aux), sep="")
  i <- i+1
}

# transforma listas em um único df
obras <- bind_rows(lista_df)

## corrige uns NAs
obras$linksAux[obras$id==1365 & (is.na(obras$linksAux) | grepl("NA", obras$linksAux ))] <-
  c("Prazo de Vigência1:", "Data de Término do Contrato1:", "Total da Planilha Contratada1:" )

## spread
obras1 <- obras %>%
  spread(linksAux, linksAux1)

## criando banco relacional
## tabela de obras

## selecionando colunas de tab_obras
vec_colunas_obras <- c(which(grepl("Município", names(obras1))),
                 which(grepl("Endereço", names(obras1))),
                 which(grepl("Termo/Convênio", names(obras1))))

## tabela obras
base_obras <- obras1 %>%
  select(c(1:4, vec_colunas_obras))

# tabela obra_status (pode varia no tempo)
base_obras_status <- obras1 %>%
  select(which(grepl("Situação", names(obras1))))     

# tabela empresas
vec_colunas_empresas <- c(which(grepl("Empresa Contratada", names(obras1))),
                       which(grepl("Endereço", names(obras1))),
                       which(grepl("Termo/Convênio", names(obras1))))

base_empresas <- obras1 %>%
  select(which(grepl("Situação", names(obras1))))         

# 
names(base_obras)[2] <- "area_construida_m2"
names(base_obras) <- gsub(":", "", names(base_obras))
base_obras$area_construida_m2 <- gsub(" m2", "", base_obras$area_construida_m2)
base_obras$area_construida_m2_1 <- as.numeric(base_obras$area_construida_m2)
head(base_obras)
dim(obras1)
names(obras1)
