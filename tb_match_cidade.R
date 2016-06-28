

remove_acento <- function(vec, Toupper=F) {
  vec <- tolower(vec)
  vec <- gsub('á', 'a', vec) 
  vec <- gsub('ã', 'a', vec)
  vec <- gsub('à', 'a', vec)
  vec <- gsub('â', 'a', vec)
  vec <- gsub('é', 'e', vec) 
  vec <- gsub('ê', 'e', vec)
  vec <- gsub('í', 'i', vec)
  vec <- gsub('ó', 'o', vec) 
  vec <- gsub('ô', 'o', vec)
  vec <- gsub('õ', 'o', vec)
  vec <- gsub('ú', 'u', vec)
  vec <- gsub('ç', 'c', vec)
#  vec <- gsub('\'', '', vec)
  if ( Toupper==T) vec <- toupper(vec)
  return(vec)
}

Sys.setlocale(category = "LC_ALL", locale = "pt_BR")

library(dplyr)
setwd("/Users/natalia/Documents/Manoel")

#educ1 <- read.table("tb_cidades.txt", header=T, as.is=T, na.strings = "", sep = "\t")
educ <- read.table("tb_cidades.txt", header=T, as.is=T, na.strings = "", sep = "\t")

escolas <- read.table("Escolas_Brasil_v3.csv", header=T, as.is=T, sep = ",",  quote="\"", na.strings = "NA",
                      fileEncoding = "UTF-8") 
head(escolas)

escolas_am  <- filter(escolas, UF == 'AM')
head(escolas_am)
escolas_am <- escolas_am %>%
  mutate(Termino_previsto1 = as.Date(Termino_previsto, "%d/%m/%Y"),
         bol_atrasada = ifelse(is.na(Termino_previsto1), 0,
                               ifelse(Termino_previsto1 < as.Date("2015-08-01"), 1, 0)))

result <- escolas_am %>%
  group_by(Situacao_obra, bol_atrasada) %>%
  summarise(n())


filter(escolas_am, Situacao_obra %in% c("Inacabada", "Execução", "Paralisada", "Concluída")) %>%
group_by(Situacao_obra, bol_atrasada) %>%
summarise(n())

filter(escolas_am, Situacao_obra %in% c("Inacabada", "Execução", "Paralisada", "Concluída")) %>%
  summarise(n())

head(educ)


summary(educ)

# preparando os dados

## mudando nome das variáveis e transformando habitantes_osb em número, pois veio como texto e retirando nomes repetidos de cidade em amarribo
educ <- educ %>%
  rename( habitantes_osb = habitantes.1 ) %>%
  mutate( habitantes_osb = as.numeric(gsub(",", "", habitantes_osb) )) %>%
  filter(  !is.na(amarribo), habitantes > 0) ## retirando cidades com NA e com zero, pois repete.

## remover acento
educ <- educ
educ$amarribo <- remove_acento(educ$amarribo)
educ$osb <- remove_acento(educ$osb)
educ$abrinqTotal <- remove_acento(educ$abrinqTotal)
educ$abrinqPremium <- remove_acento(educ$abrinqPremium)

View(educ)
# primeira tarefa

# várias formas de fazer


## Com o que vocês já aprenderam (sort of)
amarribo <- educ %>%
  dplyr::select(amarribo, habitantes) %>%
  rename( cidades = amarribo)

osb <- educ %>%
  dplyr::select(osb, habitantes_osb) %>%
  rename( cidades = osb)

# join

cidades_match1 <- amarribo %>%
  full_join(., osb, by= "cidades") %>%
  filter(!is.na(cidades) ) ## por alguma razão, algumas linhas com tudo NA. tirei essas linhas



head(cidades_match1)

# qtas cidades osb + amarribo
length(unique(cidades_match1$cidades))
# 275


tail(cidades_match1$cidades)
View(cidades_match1)
# 277 cidades

# quantos habitantes?
## preciso somar, mas cuidado com o NA
## vou retirar o NA, e substituit por 0
## função replace_na, é nova.
library(tidyr)
cidades_match1 <- cidades_match1 %>%
  replace_na(list(habitantes=0, habitantes_osb = 0))

cidades_match1 <- cidades_match1 %>%
  mutate(soma_habitante = habitantes + habitantes_osb)

cidades_match1 %>%
  summarise(sum(soma_habitante))
# resp. 93.751.635 habitantes


# Quantas cidades da Rede OSB + Amarribo estão na lista Abrinq Total?
# usar join de novo
# mas dessas vez, left_join
# left join deixa todos da primeira tabela que estou juntando com segunda, mas nada que está na segunda e não está na primeira
# vou usar sintaxe diferente. Menos linhas de código.

cidades_match2 <- educ %>%
  select(abrinqTotal, abrinqPremium) %>%
  mutate(cidades = abrinqTotal) %>% # criando nova coluna, cidades, pra fazer join.
  inner_join(., cidades_match1, by= "cidades") %>%
  arrange(abrinqTotal, abrinqPremium, cidades)

write.table(cidades_match2, file= "cidades_abrintotal_match_amarribo_osb.csv", sep=",", row.names=F)
head(cidades_match2)
# resp:
nrow(cidades_match2)
# 12


#Quantas dessas cidades são Premium?
cidades_match3 <- cidades_match2 %>%
  select ( cidades = abrinqPremium) %>%
  inner_join(. , cidades_match2, by= "cidades")


# resp:  
nrow(cidades_match3)
# 0

# Qual o total de habitantes das cidades na interseção entre Rede OSB + Amarribo e Abrinq Total?

sum(cidades_match2$soma_habitante)
# 3.616.685

