# criação das tabelas

## criando banco relacional
## tabela de obras

library(dplyr)
library(tidyr)
## carregndo base
setwd("/Users/natalia/Documents/Manoel")
load(file="obras.RData")

## selecionando colunas de tab_obras
vec_colunas_obras <- c(which(grepl("Município", names(obras1))),
                       which(grepl("Endereço", names(obras1))),
                       which(grepl("Termo/Convênio", names(obras1))))

## tabela obras
base_obras <- obras1 %>%
  select(c(1:4, vec_colunas_obras))

# modificando nomes da tabela base_obras
names(base_obras)[2] <- "area_construida_m2"
names(base_obras)[7:9] <- c("fim_termo_convenio", "situacao_termo_convenio", "termo_convenio")
names(base_obras) <- gsub(":", "", names(base_obras))
base_obras$area_construida_m2 <- gsub(" m2", "", base_obras$area_construida_m2)
base_obras$area_construida_m2 <- as.numeric(base_obras$area_construida_m2)

base_obras <- base_obras %>%
  mutate(fim_termo_convenio = replace(fim_termo_convenio, fim_termo_convenio=="-", NA),
         situacao_termo_convenio = replace(situacao_termo_convenio, situacao_termo_convenio=="-", NA),
         termo_convenio = replace(termo_convenio, termo_convenio=="-", NA))


id_valor_contrato <- obras %>%
  group_by(id) %>%
  filter(linksAux == "Valor do Contrato:",
         !duplicated(linksAux)) %>%
  mutate(linksAux = gsub("Valor do Contrato:", "valor_contrato", linksAux),
         linksAux1 = gsub("R\\$ ", "", linksAux1),
         linksAux1 = gsub("\\.", "", linksAux1),
         linksAux1 = gsub("Ver histórico de aditivos", "", linksAux1),
         linksAux1 = as.numeric(gsub(",", ".", linksAux1))) %>%
  spread(linksAux, linksAux1)

base_escolas <- select(escolas, which(names(escolas) %in% names(escolas)[c(9,12,14)]))
base_obras <- base_obras %>%
  left_join(id_valor_contrato, by="id") %>%
  left_join(base_escolas, by="id")

# tabela obra_status (pode variar no tempo)
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

obra_aditivos1 <- bind_rows(lista_tabela_aditivo)

obra_aditivos <- obra_aditivos1 %>% 
  filter(!is.na(termo_convenio), !is.na(aditivo)) %>%
  mutate(aditivo_id = 1:n())


