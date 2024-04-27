# Carregando bibliotecas utilizadas
library(dplyr)
library(scales)
library(ggplot2)


# Carregado conjunto de dados
dados = read.csv('dataset.csv')


# Normalizando coluna com o nível de obesidade
names(dados)[names(dados) == "NObeyesdad"] <- "Nivel_obesidade"
dados$Nivel_obesidade = recode(
  dados$Nivel_obesidade,
  "Insufficient_Weight" = "Abaixo do Normal",
  "Normal_Weight" = "Peso Normal",
  "Overweight_Level_I" = "Sobrepeso Nível I",
  "Overweight_Level_II" = "Sobrepeso Nível II",
  "Obesity_Type_I" = "Obesidade Tipo I",
  "Obesity_Type_II" = "Obesidade Tipo II",
  "Obesity_Type_III" = "Obesidade Tipo III"
)
# Definindo a ordem desejada para as visualizações
ordem_nivel_obesidade <- c(
  "Abaixo do Normal", "Peso Normal",
  "Sobrepeso Nível I", "Sobrepeso Nível II",
  "Obesidade Tipo I", "Obesidade Tipo II", 
  "Obesidade Tipo III"
)
dados$Nivel_obesidade <- factor(dados$Nivel_obesidade, levels = ordem_nivel_obesidade)


# Contagem geral de cada classificação de peso
contagem_tipo = table(dados$Nivel_obesidade)
contagem_tipo
# Exibindo em um gráfico de pizza com as respectivas percentagens
cores = c("#000080", "#FFFFFF", "#FF6347", "#FF0000", "#c501e2", "#760188", "#5800e6")
proporcoes = prop.table(contagem_tipo)
percentagens = paste0(round(proporcoes * 100), "%")
rotulos = paste(names(proporcoes), percentagens)
grafico_pizza = pie(
  contagem_tipo,
  main = "Percentual de cada classificação de peso",
  col = cores,
  labels = rotulos
)

# Verificando a quantidade por genero em cada classificação de peso
dados$Gender = case_match(dados$Gender, 'Female'~'Feminino', 'Male'~'Masculino')
classificacao_por_genero = dados %>%
  group_by(Nivel_obesidade, Gender) %>%
  summarize(Quantidade = n(), .groups = 'drop')

ggplot(classificacao_por_genero, aes(x = Nivel_obesidade, y = Quantidade, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Nível de Obesidade", y = "Quantidade", fill = "Gênero") +
  ggtitle("Contagem por classificação de peso e gênero") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Análise de hábitos de Obesos
# Gerando sub-conjunto apenas com dados sobre pessoas obesas
dados_obesos = dados %>%
  filter(Nivel_obesidade %in% c("Obesidade Tipo I", "Obesidade Tipo II", "Obesidade Tipo III"))

# Percentual de obesos que consomem com frequência alimentos calóricos
dados_obesos$FAVC <- case_match(dados_obesos$FAVC, 'no'~'Não', 'yes'~'Sim')
proporcoes_favc <- prop.table(table(dados_obesos$FAVC)) * 100
percentagens_favc <- paste0(round(proporcoes_favc), "%")
rotulos_favc <- paste(names(proporcoes_favc), percentagens_favc)
grafico_pizza_favc <- pie(
  proporcoes_favc,
  main = "Percentual de Consumo Frequente de Alimentos Calóricos",
  col = c("#FF0000", "#7cff22"),
  labels = rotulos_favc
)
