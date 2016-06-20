## Análise

## lista de tabelas:

# obra_aditivos
# base_contratos
# base_empresas
# base_obras_status
# base_obras

str_extract(base_obra_convenio$termo_convenio[1], "\\/[0-9]+")

library(stringr)
library(ggplot2)

## pegando obras com convênios juntando com custo das obras
base_obra_convenio <- base_obras %>%
  filter(!is.na(termo_convenio), !is.na(area_construida_m2)) %>%
  mutate(valor_contrato_m2 = valor_contrato/area_construida_m2,
         data_convenio = gsub("\\/", "", str_extract(termo_convenio, "\\/[0-9]+")))


gasto_obra <- base_obra_convenio %>%
  filter(Situacao_obra_2015 %in% c("Execução", "Paralisada", "Concluída",
                                   "Contratação", "Obra Aprovada", "Inacabada" ),
         area_construida_m2 > 0) %>%
  group_by(Situacao_obra_2015, data_convenio) %>%
  summarise(gasto_medio = mean(valor_contrato_m2, na.rm=T),
            gasto_mediano = median(valor_contrato_m2))

minus_date <- as.numeric(Sys.Date() - as.Date("2016-01-02"))
gasto_obra %>%
  mutate(data = as.Date(data_convenio, "%Y") - minus_date) %>%
  ggplot(aes(y = gasto_mediano, x=data, colour=Situacao_obra_2015)) + geom_line()


### 
# 830001/2007
filter(base_obras, termo_convenio == "830001/2007")
