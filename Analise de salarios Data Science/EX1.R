dados = read.csv2("dados_salarios.csv")

"
QUESTAO 1
Ano: quantitativa discreta
Experiência: qualitativa ordinal
Emprego: qualititativa nominal
Cargo: qualitativa nominal
Salario: qualitativa discreta
País empregado: qualitativa nominal
Trabalho remoto: qualitativa ordinal
País da empresa: qualitativa nominal
Tamanho da empresa: qualitativa ordinal
"

# QUESTAO 2
library(dplyr)
dados$trab_remoto = case_match(dados$trab_remoto, 0 ~ "Não", 50 ~ "Parcial", 100 ~ "Sim")

# QUESTAO 3
library(ggplot2)
tabela_experiencia = table(dados$experiencia)

dados$experiencia <- factor(dados$experiencia, levels = c("EN", "MI", "SE", "EX"))
ggplot(dados, aes(x=experiencia)) +
  geom_bar(color="skyblue", fill=rgb(0.1,0.6,0.7,0.7) ) +
  labs(title = "Número de profissionais por nível de experiência", x = "Nível de experiência", y = "Número de profissionais")

"
Conclusão: Percebe-se que, a maioria da amostra dos dados é composta por seniores e a minoria por executivos.
Além disso, o pequeno número de trabalhadores empregados com nível básico de conhecimento,
o que pode indicar que os empregos na área de Dados requerem um bom nível de conhecimento.
"

# QUESTAO 4
dados$trab_remoto = case_match(dados$trab_remoto, "Não" ~ "Presencial", "Parcial" ~ "Hibrido", "Sim" ~ "Remoto")
ggplot(dados, aes(x=experiencia, fill=trab_remoto)) +
  geom_bar(position="fill") +
  labs(title="Relação entre experiência e trabalho remoto", x="Nível", y="", fill = "Legenda") +
  scale_fill_manual(values = c("Presencial" = "blue", "Remoto" = "skyblue", "Hibrido" = "darkgrey")) +
  scale_y_continuous(labels = scales::percent_format())

"
Conclusão: 
"

# QUESTAO 5
tabela.medias = aggregate(dados$salario_USD, by=list(dados$experiencia), FUN="mean")
colnames(tabela.medias) = c("Experiencia","Media Salarial")
tabela.medias

"
Conclusão: 
"

# QUESTAO 6 geom_histogram ou geom_boxplot
menor_valor <- min(dados$salario_USD)
Q1 <- quantile(dados$salario_USD, 0.25)
Q3 <- quantile(dados$salario_USD, 0.75)
Q4 <- max(dados$salario_USD)

ggplot(dados, aes(x = "", y = salario_USD)) +
  geom_boxplot(color = "darkgrey", fill = "skyblue") +
  labs(title = "Distribuição de Salários em USD",
       x = "",
       y = "Salário (USD)") +
  scale_x_discrete(labels = "") +  # Escondendo os rótulos no eixo x
  geom_text(aes(x = 1, y = menor_valor, label = paste("Menor salário:", menor_valor)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = Q1, label = paste("Q1:", Q1)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = Q3, label = paste("Q3:", Q3)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = Q4, label = paste("Maior salário:", Q4)), vjust = -0.5, color = "blue") +
  scale_y_continuous(labels = scales::comma)  # Adicionando vírgulas nos rótulos do eixo y

"
Conclusão: Pq um boxplot, com esse grafico é possivel perceber q o menor valor é R$2859, 
e 50% dos salarios estão nesses grandes retangulos, ou seja, entre R$ 62.726 - R$ 150.000,
pode-se perceber, também q esses pontinhos são valores anomalos, que estão todos acima de R$ 300.000.
"
