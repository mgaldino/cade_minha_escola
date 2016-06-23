### validação

length(unique(obras_aditivo$id))
length(unique(obras_aditivo$linksAux1[obras_aditivo$linksAux == "Termo/Convênio:"]))
##  9523 convênios distintos

conv_traco <- obras_aditivo %>%
  filter(linksAux == "Termo/Convênio:", grepl("-", linksAux1))
View(conv_traco)

length(unique(obras_aditivo1$id))
length(unique(obras_aditivo1$linksAux1[obras_aditivo1$linksAux == "Termo/Convênio:"]))

h2 <- obras %>%
  filter(id %in% conv_traco$id,
         linksAux == "Valor do Contrato:",
         linksAux == "Data de Assinatura do Contrato:")

head(h2)
View(h2)

teste <- filter(base_obras, termo_convenio == "830001/2007")
teste1 <- filter(obra_aditivos, termo_convenio == "830001/2007" )
teste2 <- obras %>% filter(id == 1603) 
teste3 <- base_obra_convenio %>% filter(id == 1603) ## valor do contraro original está errado


teste2_aditivo <- teste2 %>%
  do(cria_aditivo(.)) 

View(teste2_aditivo)
## validação
head(obra_aditivos, 9)
dim(obra_aditivos)
length(unique(obra_aditivos$termo_convenio))
length(unique(obras$linksAux1[obras$linksAux == "Valor do Contrato:"]))


val <- obras %>%
  group_by(id) %>%
  summarise(denominacao = )
filter(linksAux ==  "Denominação:")

  

h <- obras %>%
  filter(id == 12265)
View(h)

h1 <- obras %>%
  filter(grepl("Dois Irmãos", linksAux1 ),
         grepl("RS", linksAux1 ),
         grepl("Município - UF:", linksAux))
head(h1)
View(h1)
