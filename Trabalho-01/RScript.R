# Carregando bibliotecas utilizadas
library(dplyr)
library(scales)


# Carregado conjunto de dados
dados = read.csv('dataset.csv')


# Normalizando coluna com o nível de obesidade
names(dados)[names(dados) == "NObeyesdad"] <- "Nivel_obesidade"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Insufficient_Weight"] <- "Abaixo do Normal"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Normal_Weight"] <- "Peso Normal"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Overweight_Level_I"] <- "Sobrepeso Nível I"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Overweight_Level_II"] <- "Sobrepeso Nível II"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Obesity_Type_I"] <- "Obesidade Tipo I"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Obesity_Type_II"] <- "Obesidade Tipo II"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Obesity_Type_II"] <- "Obesidade Tipo III"

# Contagem geral da quantidade de cada tipo
contagem_tipo = table(dados$Nivel_obesidade)
contagem_tipo
# Exibindo em um gráfico de pizza com as respectivas percentagens
cores = c("#000080", "#EE82EE", "#7B68EE", "#4B0082", "#DCDCDC", "#FF6347", "#FF0000")
proporcoes = prop.table(contagem_tipo)
percentagens = paste0(round(proporcoes * 100), "%")
rotulos = paste(names(proporcoes), percentagens)
pie(
  contagem_tipo,
  main = "Distribuição de Tipos de Obesidade",
  col = cores,
  labels = rotulos
)

