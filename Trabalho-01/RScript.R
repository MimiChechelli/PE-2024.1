# Carregando bibliotecas utilizadas
library(dplyr)
library(scales)
library(ggplot2)

# Carregado conjunto de dados
dados = read.csv('dataset.csv')

# Normalizando coluna com o nível de obesidade
names(dados)[names(dados) == "NObeyesdad"] <- "Nivel_obesidade"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Insufficient_Weight"] <- "Abaixo"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Normal_Weight"] <- "Normal"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Overweight_Level_I"] <- "Sobrepeso I"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Overweight_Level_II"] <- "Sobrepeso II"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Obesity_Type_I"] <- "Obesidade I"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Obesity_Type_II"] <- "Obesidade II"
dados$Nivel_obesidade[dados$Nivel_obesidade == "Obesity_Type_III"] <- "Obesidade III"

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

# Relação entre pessoas com/sem pessoas obesas na familia e seu nível de obesidade
relação_nivel_familia <- dados %>%
  group_by(family_history_with_overweight, Nivel_obesidade) %>%
  summarise(numero_de_casos = n())

# Criação do gráfico de barras
relação_nivel_familia$family_history_with_overweight[relação_nivel_familia$family_history_with_overweight == "yes"] <- "presente"
relação_nivel_familia$family_history_with_overweight[relação_nivel_familia$family_history_with_overweight == "no"] <- "ausente"

ggplot(relação_nivel_familia, aes(factor(Nivel_obesidade, levels = c("Abaixo", "Normal", "Sobrepeso I", "Sobrepeso II", "Obesidade I", "Obesidade II", "Obesidade III")), y = numero_de_casos, fill = family_history_with_overweight)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nível de Obesidade por Histórico Familiar de Obesidade",
       x = "Nível de Obesidade",
       y = "Número de Casos",
       fill = "Histórico Familiar")

# Conclusão:
# Podemos notar que pessoas com níveis abaixo e normal possuem uma quantidade muito parecida entre ter e não ter parentes
# obesos, conforme o nível de obesidade aumenta podemos ver que o numero de pessoas com ausencia de pessoas obesas
# na familia se extingue o que abre questionamento sobre os habitos dessas familias 

# descobrindo os habitos de pessoas obesas 
acima_de_normal <- dados[dados$Nivel_obesidade != "Normal" & dados$Nivel_obesidade != "Abaixo", ]









