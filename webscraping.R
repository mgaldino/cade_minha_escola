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
  filter(!(linksAux %in% c("Valor do Contrato:", "Denominação:", "Tipo de Aditivo:" , 
                           "Data de Assinatura do Aditivo:", "Justificativa do Aditivo:",
                           "Prazo de Vigência:", "Data de Término do Contrato:"))) %>%
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

# modificando nomes da tabela base_obras
names(base_obras)[2] <- "area_construida_m2"
names(base_obras) <- gsub(":", "", names(base_obras))
base_obras$area_construida_m2 <- gsub(" m2", "", base_obras$area_construida_m2)
base_obras$area_construida_m2_1 <- as.numeric(base_obras$area_construida_m2)
head(base_obras)


# tabela obra_status (pode varia no tempo)
base_obras_status <- obras1 %>%
  select(which(grepl("Situação", names(obras1))))     

# tabela empresas
vec_colunas_empresas <- c(which(grepl("Empresa Contratada", names(obras1))),
                       which(grepl("Endereço", names(obras1))),
                       which(grepl("Termo/Convênio", names(obras1))))

base_empresas <- obras1 %>%
  select(which(grepl("Situação", names(obras1))))         

## tabela contrato
base_contratos <- obras1 %>%
  select(which(grepl("Situação", names(obras1))))         


# coluna aditivo
cria_aditivo <- function(df) {
  df$aditivo <- NA
  if(sum(grepl("Denominação:", df$linksAux)) > 0) {
    aux_aditivo <- which(df$linksAux=="Denominação:")
    df$aditivo[aux_aditivo] <-  1:length(aux_aditivo)
    index_aditivo <- c(which(!is.na(df$aditivo)), length(df$aditivo))
    n <- length(index_aditivo) -1
    for ( i in 1:n) {
      df$aditivo[(index_aditivo[i]-1):index_aditivo[i+1]] <- i
    }
  }

  return(df)
}

obras$linksAux <- gsub("[0-9]","", obras$linksAux)

obras_aditivo <- obras %>%
  group_by(id) %>%
  do(cria_aditivo(.)) %>%
  ungroup()

cria_colunas_adicionais_aditivo <- function(df) {
  df$tipo_aditivo <- NA
  df$denominacao <- NA
  df$data_termino_contrato <- NA
  df$termo_convenio <- NA
  df$valor_contrato <- NA
  
  if(sum(grepl("Denominação:", df$linksAux)) > 0) {
    # coluna tipo aditivo
    aux_tipo_aditivo <- which(df$linksAux=="Tipo de Aditivo:")
    df$tipo_aditivo[aux_tipo_aditivo] <-  df$linksAux1[aux_tipo_aditivo]
    
    # coluna demoninação
    aux_denominacao <- which(df$linksAux=="Denominação:")
    df$denominacao[aux_denominacao] <-  df$linksAux1[aux_denominacao]
    
    # coluna data termino contrato
    aux_data_termino_contrato <- which(df$linksAux=="Data de Término do Contrato:")
    df$data_termino_contrato[aux_data_termino_contrato] <-  df$linksAux1[aux_data_termino_contrato]
    
    ## coluna termo_convenio
    aux_termo_convenio <- which(df$linksAux=="Termo/Convênio:")
    df$termo_convenio[aux_termo_convenio] <-  df$linksAux1[aux_termo_convenio]
    
    ## coluna valor_contrato
    aux_valor_contraro <- which(df$linksAux=="Valor do Contrato:")
    df$valor_contrato[aux_valor_contraro] <- df$linksAux1[aux_valor_contraro]
    
  }

  return(df)
}

obras_aditivo1 <- obras_aditivo %>%
  group_by(id) %>%
  do(cria_colunas_adicionais_aditivo(.)) %>%
  ungroup()

## criando tabela aditivo
cria_tabela_aditivo <- function(df) {
  stopifnot(require(dplyr))
  
  if (sum(grepl("Denominação:", df$linksAux)) > 0 ) {
    aditivo <- df %>%
      select_("aditivo:valor_contrato") %>%
      filter_(!is.na("aditivo") | !is.na("termo_convenio")) %>%
      group_by_("aditivo") %>%
      summarise_(tipo_aditivo = ~max(tipo_aditivo, na.rm=T),
                 data_termino_contrato = ~max(data_termino_contrato, na.rm=T),
                 denominacao = ~max(denominacao, na.rm=T),
                 termo_convenio = ~max(termo_convenio, na.rm=T),
                 valor_contrato = ~max(valor_contrato, na.rm=T)) %>%
      mutate_(termo_convenio = ~max(termo_convenio, na.rm=T)) %>%
      filter_(!is.na("aditivo") ) 
  } else {
    aditivo <- data.frame(aditivo=NA, tipo_aditivo = NA, data_termino_contrato = NA, 
                          denominacao = NA, valor_contrato = NA, termo_convenio = unique(df$termo_convenio))
  }

  return(aditivo)
}

lista_tabela_aditivo <- list()
vec <- unique(obras$id)
n <- length(vec)
for (i in 1:n) {
  tmp_table <- filter(obras_aditivo1, id == vec[i])
  lista_tabela_aditivo[[i]] <- cria_tabela_aditivo(tmp_table)
}

obra_aditivos <- bind_rows(lista_tabela_aditivo) %>%
  filter(!is.na(termo_convenio), !is.na(aditivo)) %>%
  mutate(aditivo_id = 1:n())

head(obra_aditivos, 9)
dim(obra_aditivos)
length(unique(obra_aditivos$termo_convenio))

save(file="obras.RData", obras)

obra_aditivos %>%
  mutate(valor_contrato = gsub("R\\$", "", valor_contrato),
         valor_contrato = gsub("\\.", "", valor_contrato),
         valor_contrato = gsub(",", ".", valor_contrato),
         valor_contrato = as.numeric(valor_contrato)) %>%
  group_by(tipo_aditivo) %>%
  summarise(media=mean(valor_contrato))

