# Exercicio3.R
## Alunos: Davi Rodrigues Chiqueti, Michelli Patricia Luvison, Renan dos Santos Silva
### 2024-04-19

# Leitura dos dados
dados <- read.csv2("mobile.csv", dec=".")

# Seleciona uma amostra ao acaso com 600 observações
library(dplyr)
set.seed(123) # Garante a reprodutibilidade da amostra
amostra <- dados %>% sample_n(600) 

# Questão 1: Calcule a média, desvio padrão e coeficiente de variação para as seguintes variáveis:
## A: battery_power em função de touch_screen
bateria_total <- amostra %>% 
  group_by(touch_screen) %>% 
  summarise(
    media = mean(battery_power),
    desvio_padrao = sd(battery_power),
    coeficiente_variacao = (sd(battery_power) / mean(battery_power)) * 100
  )
# Conclusão:
## 
## 
## 

# B: m_dep em função de touch_screen
profundidade <- amostra %>% 
  group_by(touch_screen) %>% 
  summarise(
    media = mean(m_dep),
    desvio_padrao = sd(m_dep),
    coef_variacao = (sd(m_dep) / mean(m_dep)) * 100
  )
# Conclusão:
## 
## 
## 

# C: int_memory em função de blue
memoria_interna <- amostra %>% 
  group_by(blue) %>% 
  summarise(
    media = mean(int_memory),
    desvio_padrao = sd(int_memory),
    coef_variacao = (sd(int_memory) / mean(int_memory)) * 100
  )
# Conclusão:
## 
##
##

library(ggplot2)
# Gráfico do item 1.a
ggplot(amostra, aes(x = as.factor(touch_screen), y = battery_power)) +
  geom_boxplot() +
  labs(title = "Boxplot relacionando a vida da bateria por tela tocável", x = "Tela tocável", y = "Vida da bateria")

# Gráfico do item 1.b
ggplot(amostra, aes(x = as.factor(touch_screen), y = m_dep)) +
  geom_boxplot() +
  labs(title = "Boxplot relacionando a profundidade por tela tocável", x = "Touch Screen", y = "Mobile Depth")

# Gráfico do item 1.c
ggplot(amostra, aes(x = as.factor(blue), y = int_memory)) +
  geom_boxplot() +
  labs(title = "Boxplot relacionando a memória interna por bluetooth", x = "Bluetooth", y = "Internal Memory")

