# Carregando e descrevendo o conjunto de dados
df = read.csv2("dados_salarios.csv")
summary(df)

# QUESTÃO 1
amostra = df[sample(nrow(df), 300),]
show(amostra)


# QUESTÃO 2
media = mean(amostra$salario_USD)
media
mediana = median(amostra$salario_USD)
mediana
percentil_5 = quantile(amostra$salario_USD, 0.05)
percentil_5
percentil_25 = quantile(amostra$salario_USD, 0.25)
percentil_25
percentil_75 = quantile(amostra$salario_USD, 0.75)
percentil_75
percentil_95 = quantile(amostra$salario_USD, 0.95)
percentil_95
min_amostra = min(amostra$salario_USD)
min_amostra
max_amostra = max(amostra$salario_USD)
max_amostra
# Conclusão: Considerando a mediana muito abaixo entre o ponto central entre mínimo e máximo,
# podemos perceber uma assimetria negativa (ou à esquerda) na distribuição da amostra.
# Sugerindo que a maior parte dos funcionários recebe menos do que a metade do maior sálario da amostra.


# QUESTÃO 3
library(dplyr)
amostra$trab_remoto = case_match(amostra$trab_remoto, 0 ~ "Presencial", 50 ~ "Hibrido", 100 ~ "Remoto")
percentis_por_grupo_trabalho <- tapply(amostra$salario_USD, amostra$trab_remoto, function(x) quantile(x, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)))
percentis_por_grupo_trabalho
# Conclusão: O grupo do trabalho remoto foi o grupo que apresentou sálarios maiores em relação aos outros.
# Com destaque para a diferença drástica ao sálario máximo.
# Seu maior salário é maior do que o dobro do maior salário registrado por um funcionário que trabalha presencialmente.
# indique se é melhor usar a média ou mediana na comparação

# QUESTÃO 4
percentis_por_grupo_empresa <- tapply(amostra$salario_USD, amostra$tam_empresa, function(x) quantile(x, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)))
# Conclusão: ...
# indique se é melhor usar a média ou mediana na comparação


# Questão 5
amostra$tam_empresa = case_match(amostra$tam_empresa, "S" ~ "Pequena", "M" ~ "Média", "L" ~ "Grande")
percentis_empresa_pequena = percentis_por_grupo_empresa$S
percentis_empresa_media = percentis_por_grupo_empresa$M
percentis_empresa_grande = percentis_por_grupo_empresa$L
# Plot the percentiles
range = range(0, max(amostra$salario_USD))
custom_y_ticks = pretty(range, n=8)
plot(
  percentis_por_grupo_empresa$S, type = "l", col = "red",
  xlab = "Percentil", ylab = "Salário", main = "Percentils por tamanho da empresa",
  xaxt="n", yaxt="n", ylim = c(min(custom_y_ticks), max(custom_y_ticks))
)
lines(percentis_por_grupo_empresa$M, col = "green")
lines(percentis_por_grupo_empresa$L, col = "blue")
axis(1, at = c(1, 2, 3, 4, 5, 6, 7), labels = c("mínimo", 5, 25, "50 - mediana", 75, 95, "máximo"))
axis(2, at = custom_y_ticks, labels = custom_y_ticks, las=2)
legend("topleft", legend = c("Pequena", "Média", "Grande"), col = c("red", "green", "blue"), lty = 1)
par(mar = c(5, 6, 4, 4) + 0.1)
options(scipen=999)
grid()

