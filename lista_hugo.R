## Gerando base ciadades para pedido LAI
## info sobre fiscal da obra, cronograma financeiro e de execução
## e relatório do fiscal.

setwd("/Users/natalia/Documents/Manoel")
load(file="base_obras.RData")

library(stringr)

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
write.table(aux, file="amostra_pedido_lai.csv", row.names=F)
